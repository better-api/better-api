//! Defines semantic representation of types.
//!
//! The main data structure is a [`TypeArena`] that holds semantic types
//! resolved from syntax. Types in the arena are referenced with [`TypeId`],
//! and every stored value is [tracked](crate::Tracked) so we can map it back
//! to the originating SyntaxNode.
//!
//! ## Building An Arena
//!
//! [Simple types](SimpleType) can be added directly with
//! [`TypeArena::add_simple`]. Composite types (records and unions) are built
//! through the builder API returned by [`TypeArena::start_record`] and
//! [`TypeArena::start_union`]. Builders allow us to build the composite types
//! without additional heap allocations.
//!
//! ## Getting Types
//!
//! To retrieve a type, pass a [`TypeId`] to [`TypeArena::get`]. This returns a
//! [`TrackedType`], giving access to both semantic data and the syntax pointer
//! required for diagnostics.
//!
//! ## Strings and Names
//!
//! Some types hold a [`&Name`](Name) which is a wrapper around a &str.
//! Ownership of names and strings should be handled by [`StringCache`](crate::text::StringCache)
//!
//! ## Lifetimes
//!
//! - `'s` captures the lifetime of string slices managed by the [`StringCache`](crate::text::StringCache).
//! - `'a` ties a type view to the lifetime of the [`TypeArena`].
use better_api_syntax::SyntaxNode;

use crate::Tracked;
use crate::name::Name;
use crate::value;

/// Representation of a type.
#[derive(Debug, PartialEq)]
pub enum Type<'s, 'a> {
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Date,
    Timestamp,
    Bool,
    String,
    File,
    Reference(Reference<'s, 'a>),
    Option(Reference<'s, 'a>),
    Array(Reference<'s, 'a>),
    Record(TypeFieldIterator<'s, 'a>),
    Union(Union<'s, 'a>),
    Enum(Enum<'s, 'a>),
    Response(Response<'s, 'a>),
}

impl<'s, 'a> Type<'s, 'a> {
    fn from_simple(arena: &'a TypeArena<'s>, simple: SimpleType) -> Self {
        match simple {
            SimpleType::I32 => Type::I32,
            SimpleType::I64 => Type::I64,
            SimpleType::U32 => Type::U32,
            SimpleType::U64 => Type::U64,
            SimpleType::F32 => Type::F32,
            SimpleType::F64 => Type::F64,
            SimpleType::Date => Type::Date,
            SimpleType::Timestamp => Type::Timestamp,
            SimpleType::Bool => Type::Bool,
            SimpleType::String => Type::String,
            SimpleType::File => Type::File,
            SimpleType::Reference(id) => Type::Reference(Reference { arena, id }),
            SimpleType::Option(id) => Type::Option(Reference { arena, id }),
            SimpleType::Array(id) => Type::Array(Reference { arena, id }),
            SimpleType::Enum { typ, values } => Type::Enum(Enum {
                typ: typ.map(|id| Reference { arena, id }),
                values,
            }),
            SimpleType::Response {
                body,
                headers,
                content_type,
            } => Type::Response(Response {
                body: body.map(|id| Reference { arena, id }),
                headers: headers.map(|id| Reference { arena, id }),
                content_type,
            }),
        }
    }

    fn from_slot(arena: &'a TypeArena<'s>, id: TypeId, slot: &'a Slot<'s>) -> Self {
        match slot {
            Slot::Simple(simple) => Type::from_simple(arena, *simple),
            Slot::Record { end } => Type::Record(TypeFieldIterator {
                arena,
                current: id,
                end: *end,
            }),
            Slot::Union { discriminator, end } => Type::Union(Union {
                disriminator: discriminator.clone(),
                fields: TypeFieldIterator {
                    arena,
                    current: id,
                    end: *end,
                },
            }),
            Slot::TypeField { .. } => unreachable!("invalid conversion of Slot::TypeField to Type"),
        }
    }
}

pub type TrackedType<'s, 'a> = Tracked<Type<'s, 'a>>;

/// Reference to a type stored in the [`TypeArena`].
#[derive(derive_more::Debug, PartialEq)]
pub struct Reference<'s, 'a> {
    /// Id of the type in the arena.
    pub id: TypeId,

    #[debug(skip)]
    arena: &'a TypeArena<'s>,
}

impl<'s, 'a> Reference<'s, 'a> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> TrackedType<'s, 'a> {
        self.arena.get(self.id)
    }
}

/// Field exposed by a record or union.
///
/// Default is stored in the [`value::ValueArena`].
pub struct TypeField<'s, 'a> {
    pub name: &'s Name,
    pub default: Option<value::ValueId>,
    pub typ: TrackedType<'s, 'a>,
}

pub type TrackedTypeField<'s, 'a> = Tracked<TypeField<'s, 'a>>;

/// Iterator returned for composite types.
///
/// Each item is a [`TrackedTypeField`].
#[derive(derive_more::Debug, PartialEq)]
pub struct TypeFieldIterator<'s, 'a> {
    #[debug(skip)]
    arena: &'a TypeArena<'s>,
    current: TypeId,
    end: TypeId,
}

impl<'s, 'a> Iterator for TypeFieldIterator<'s, 'a> {
    type Item = TrackedTypeField<'s, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let field_slot = &self.arena.data[self.current.0 as usize];
        let typ_slot = &self.arena.data[self.current.0 as usize + 1];

        let (name, default) = match &field_slot.data {
            Slot::TypeField { name, default } => (*name, *default),
            val => unreachable!("invalid type field in arena: {val:?}"),
        };

        let typ = Type::from_slot(self.arena, TypeId(self.current.0 + 1), &typ_slot.data);

        self.current.0 += 2;

        Some(TrackedTypeField {
            syntax: field_slot.syntax,
            data: TypeField {
                name,
                default,
                typ: TrackedType {
                    syntax: typ_slot.syntax,
                    data: typ,
                },
            },
        })
    }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through a [`TypeFieldIterator`].
#[derive(Debug, PartialEq)]
pub struct Union<'s, 'a> {
    pub disriminator: Option<Tracked<&'s Name>>,
    pub fields: TypeFieldIterator<'s, 'a>,
}

/// Semantic representation of an enum.
///
/// Optional `typ` describes the type of enum values, while `values` points to
/// an array in the [`value::ValueArena`].
#[derive(Debug, PartialEq)]
pub struct Enum<'s, 'a> {
    pub typ: Option<Reference<'s, 'a>>,
    pub values: value::ValueId,
}

/// Semantic information about a response type.
#[derive(Debug, PartialEq)]
pub struct Response<'s, 'a> {
    pub body: Option<Reference<'s, 'a>>,
    pub headers: Option<Reference<'s, 'a>>,
    pub content_type: Option<value::ValueId>,
}

/// Id of a type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

/// Simple types.
///
/// These types can be constructed without using a builder.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SimpleType {
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Date,
    Timestamp,
    Bool,
    String,
    File,
    Reference(TypeId),
    Option(TypeId),
    Array(TypeId),
    Enum {
        typ: Option<TypeId>,
        values: value::ValueId,
    },
    Response {
        body: Option<TypeId>,
        headers: Option<TypeId>,
        content_type: Option<value::ValueId>,
    },
}

/// Slot in the type arena.
#[derive(Debug, PartialEq)]
enum Slot<'s> {
    Simple(SimpleType),
    Record {
        // Id after the last field in the record.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    Union {
        discriminator: Option<Tracked<&'s Name>>,
        // Id after the last field in the union.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    // Name of the field in record or union.
    // In TrackedSlot syntax pointer points to the whole field
    // and not just the name.
    TypeField {
        name: &'s Name,
        default: Option<value::ValueId>,
    },
}

impl<'s> From<SimpleType> for Slot<'s> {
    fn from(value: SimpleType) -> Self {
        Self::Simple(value)
    }
}

type TrackedSlot<'s> = Tracked<Slot<'s>>;

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArena::start_record`] or [`TypeArena::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub struct FieldBuilder<'s, 'a> {
    arena: &'a mut TypeArena<'s>,

    /// Index in the arena that contains Slot::Record or Slot::Union.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'s, 'p> FieldBuilder<'s, 'p> {
    fn new_record(arena: &'p mut TypeArena<'s>, node: &SyntaxNode) -> Self {
        let idx = arena.data.len();
        arena
            .data
            .push(Tracked::new(node, Slot::Record { end: TypeId(0) }));

        Self {
            arena,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_union(
        arena: &'p mut TypeArena<'s>,
        node: &SyntaxNode,
        discriminator: Option<Tracked<&'s Name>>,
    ) -> Self {
        let idx = arena.data.len();
        arena.data.push(Tracked::new(
            node,
            Slot::Union {
                discriminator,
                end: TypeId(0),
            },
        ));

        Self {
            arena,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn data(&mut self) -> &mut Vec<TrackedSlot<'s>> {
        &mut self.arena.data
    }

    /// Add a new field with a simple type.
    ///
    /// `field_node` should be the node describing the entire field (name and type).
    /// `typ_node` should point to the type of the field.
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub fn add_simple(
        &mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        typ_node: &SyntaxNode,
        typ: SimpleType,
        default: Option<value::ValueId>,
    ) {
        self.data()
            .push(Tracked::new(field_node, Slot::TypeField { name, default }));
        self.data().push(Tracked::new(typ_node, typ.into()));
    }

    /// Finalize the record or union currently being built.
    ///
    /// Returns the id of the composite type inside the arena.
    pub fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = TypeId(self.data().len() as u32);

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
        match &mut head.data {
            Slot::Record { end } => *end = idx,
            Slot::Union { end, .. } => *end = idx,
            _ => unreachable!("invalid FieldBuilder start"),
        }

        self.start
    }
}

impl<'s, 'a> Drop for FieldBuilder<'s, 'a> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.data().truncate(len);
    }
}

/// Arena that holds semantic types.
#[derive(Debug, Default, PartialEq)]
pub struct TypeArena<'s> {
    data: Vec<TrackedSlot<'s>>,
}

impl<'s> TypeArena<'s> {
    /// Create a new type arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Clears the type arena, removing all types.
    ///
    /// Note that this method has no effect on the allocated
    /// capacity of the arena.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Get a type by id.
    pub fn get<'a>(&'a self, id: TypeId) -> TrackedType<'s, 'a> {
        let typ = &self.data[id.0 as usize];
        let data = Type::from_slot(self, id, &typ.data);

        TrackedType {
            syntax: typ.syntax,
            data,
        }
    }

    /// Add a simple type to the arena.
    ///
    /// Returns the [`TypeId`] assigned to the new type.
    pub fn add_simple(&mut self, node: &SyntaxNode, typ: SimpleType) -> TypeId {
        let idx = self.data.len();
        self.data.push(Tracked::new(node, typ.into()));
        TypeId(idx as u32)
    }

    /// Start building a record type.
    ///
    /// Returns a [`FieldBuilder`] rooted at the new record slot.
    pub fn start_record<'a>(&'a mut self, node: &SyntaxNode) -> FieldBuilder<'s, 'a> {
        FieldBuilder::new_record(self, node)
    }

    /// Start building a union type.
    ///
    /// Returns a [`FieldBuilder`] configured with the provided discriminator.
    pub fn start_union<'a>(
        &'a mut self,
        node: &SyntaxNode,
        discriminator: Option<Tracked<&'s Name>>,
    ) -> FieldBuilder<'s, 'a> {
        FieldBuilder::new_union(self, node, discriminator)
    }
}
