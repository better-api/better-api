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
    fn from_primitive(arena: &'a TypeArena<'s>, simple: PrimitiveType) -> Self {
        match simple {
            PrimitiveType::I32 => Type::I32,
            PrimitiveType::I64 => Type::I64,
            PrimitiveType::U32 => Type::U32,
            PrimitiveType::U64 => Type::U64,
            PrimitiveType::F32 => Type::F32,
            PrimitiveType::F64 => Type::F64,
            PrimitiveType::Date => Type::Date,
            PrimitiveType::Timestamp => Type::Timestamp,
            PrimitiveType::Bool => Type::Bool,
            PrimitiveType::String => Type::String,
            PrimitiveType::File => Type::File,
            PrimitiveType::Reference(id) => Type::Reference(Reference { arena, id }),
        }
    }

    fn from_slot(arena: &'a TypeArena<'s>, id: TypeId, slot: &'a Slot<'s>) -> Self {
        match slot {
            Slot::Primitive(simple) => Type::from_primitive(arena, *simple),
            Slot::Array { .. } => Type::Array(Reference {
                arena,
                id: TypeId(id.0 + 1),
            }),
            Slot::Option { .. } => Type::Option(Reference {
                arena,
                id: TypeId(id.0 + 1),
            }),
            Slot::Enum { typ, values } => Type::Enum(Enum {
                typ: typ.map(|id| Reference { arena, id }),
                values: *values,
            }),
            Slot::Response {
                body,
                headers,
                content_type,
            } => Type::Response(Response {
                body: body.map(|id| Reference { arena, id }),
                headers: headers.map(|id| Reference { arena, id }),
                content_type: *content_type,
            }),

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

        self.current = match &typ_slot.data {
            Slot::Option { end } => *end,
            Slot::Array { end } => *end,
            Slot::Primitive(_) => TypeId(self.current.0 + 2),
            typ => unreachable!("invalid type's field type in arena: {typ:?}"),
        };

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
pub enum PrimitiveType {
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
}

/// Slot in the type arena.
///
/// Most of the representations are obvious, the two notable that deserve a separate
/// comment are Array and Option. Array and Option can be intertwined to form a complex
/// nested type (ie. `[[[i32?]]?]?`). They can also be nested this way inside Record or Union
/// field, which means we need an efficient way to skip the whole composition.
///
/// Array and Object work by having the next type in the arena be the type wrapped in them.
/// For instance `[i32]` in arena is represented as `[Slot::Array, Slot::Primitive(_)]`
/// This allows us to nest stuff, like example above:
/// ```
/// [Slot::Option, Slot::Array, Slot::Option, Slot::Array, Slot::Array, Slot::Option,
/// Slot::Primitive(_)]
/// ```
/// The `end` property of Array and Option represent the TypeId of the type after the whole nesting
/// and can be used to skip the whole nested type when doing field iteration.
#[derive(Debug, PartialEq)]
enum Slot<'s> {
    Primitive(PrimitiveType),
    Option {
        // Id after the last type in the nested Option<...> type.
        // Used for skipping the whole Option during iteration.
        end: TypeId,
    },
    Array {
        // Id after the last type in the nested Array<...> type.
        // Used for skipping the whole Array during iteration.
        end: TypeId,
    },
    Enum {
        typ: Option<TypeId>,
        values: value::ValueId,
    },
    Response {
        body: Option<TypeId>,
        headers: Option<TypeId>,
        content_type: Option<value::ValueId>,
    },
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

impl<'s> From<PrimitiveType> for Slot<'s> {
    fn from(value: PrimitiveType) -> Self {
        Self::Primitive(value)
    }
}

type TrackedSlot<'s> = Tracked<Slot<'s>>;

trait BuilderParent<'s> {
    fn arena(&mut self) -> &mut TypeArena<'s>;

    fn data(&mut self) -> &mut Vec<TrackedSlot<'s>> {
        &mut self.arena().data
    }
}

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArena::start_record`] or [`TypeArena::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub struct FieldBuilder<'s, 'p> {
    arena: &'p mut TypeArena<'s>,

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
        typ: PrimitiveType,
        default: Option<value::ValueId>,
    ) {
        self.data()
            .push(Tracked::new(field_node, Slot::TypeField { name, default }));
        self.data().push(Tracked::new(typ_node, typ.into()));
    }

    /// Add a new field of array type.
    ///
    /// `field_node` should be the node of the whole field (name & type).
    /// `array_node` should be the node of only the array that will be built (field type).
    pub fn start_array<'a>(
        &'a mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        array_node: &SyntaxNode,
        default: Option<value::ValueId>,
    ) -> OptionArrayBuilder<'s, 'a> {
        self.data()
            .push(Tracked::new(field_node, Slot::TypeField { name, default }));

        OptionArrayBuilder::new_array(self, array_node)
    }

    /// Add a new field of option type.
    ///
    /// `field_node` should be the node of the whole field (name & type).
    /// `option_node` should be the node of only the option that will be built (field type).
    pub fn start_option<'a>(
        &'a mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        option_node: &SyntaxNode,
        default: Option<value::ValueId>,
    ) -> OptionArrayBuilder<'s, 'a> {
        self.data()
            .push(Tracked::new(field_node, Slot::TypeField { name, default }));

        OptionArrayBuilder::new_option(self, option_node)
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

impl<'s, 'a> BuilderParent<'s> for FieldBuilder<'s, 'a> {
    fn arena(&mut self) -> &mut TypeArena<'s> {
        self.arena
    }
}

/// Helper type for adding Array and Option type to arena.
///
/// This type is constructed with [`TypeArena::start_array`],
/// [`TypeArena::start_option`] or [`FieldBuilder`] equivalent.
/// After you are done, you should call [`finish`](OptionArrayBuilder::finish),
/// which returns the id of the final type in the arena.
///
/// If builder is dropped before calling finish, added types are removed from the arena.
pub struct OptionArrayBuilder<'s, 'p> {
    parent: &'p mut dyn BuilderParent<'s>,

    /// Index in the arena that contains the first Slot::Array or Slot::Option.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'s, 'p> OptionArrayBuilder<'s, 'p> {
    fn new_array(parent: &'p mut dyn BuilderParent<'s>, node: &SyntaxNode) -> Self {
        let idx = parent.data().len();
        parent
            .data()
            .push(Tracked::new(node, Slot::Array { end: TypeId(0) }));

        Self {
            parent,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_option(parent: &'p mut dyn BuilderParent<'s>, node: &SyntaxNode) -> Self {
        let idx = parent.data().len();
        parent
            .data()
            .push(Tracked::new(node, Slot::Option { end: TypeId(0) }));

        Self {
            parent,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Start a new array inside the current array or option.
    pub fn start_array(&mut self, node: &SyntaxNode) {
        self.parent
            .data()
            .push(Tracked::new(node, Slot::Array { end: TypeId(0) }));
    }

    /// Start a new option inside the current array or option.
    pub fn start_option(&mut self, node: &SyntaxNode) {
        self.parent
            .data()
            .push(Tracked::new(node, Slot::Option { end: TypeId(0) }));
    }

    /// Finish building this type.
    pub fn finish(mut self, node: &SyntaxNode, typ: PrimitiveType) -> TypeId {
        self.finished = true;

        let start = self.start.0 as usize;
        let len = self.parent.data().len();
        let end_id = TypeId(len as u32 + 1);

        // Update the in between types
        for slot in &mut self.parent.data()[start..len] {
            match &mut slot.data {
                Slot::Option { end } => *end = end_id,
                Slot::Array { end } => *end = end_id,
                _ => unreachable!(),
            }
        }

        self.parent.data().push(Tracked::new(node, typ.into()));

        end_id
    }
}

impl<'s, 'p> Drop for OptionArrayBuilder<'s, 'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.parent.data().truncate(len);
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

    /// Add a primitive type to the arena.
    ///
    /// Returns the [`TypeId`] assigned to the new type.
    pub fn add_primitive(&mut self, node: &SyntaxNode, typ: PrimitiveType) -> TypeId {
        let idx = self.data.len();
        self.data.push(Tracked::new(node, typ.into()));
        TypeId(idx as u32)
    }

    /// Add an enum to the arena.
    ///
    /// - `typ` is the type of the values in the enum.
    /// - `values` should be a [`ValueId`](value::ValueId) pointing to array of possible values for
    ///   this arena.
    /// - `node` is the syntax node of the whole enum.
    ///
    /// Returns the [`TypeId`] assigned to the enum.
    pub fn add_enum(
        &mut self,
        node: &SyntaxNode,
        typ: Option<TypeId>,
        values: value::ValueId,
    ) -> TypeId {
        let idx = self.data.len();
        self.data
            .push(Tracked::new(node, Slot::Enum { typ, values }));
        TypeId(idx as u32)
    }

    /// Add a response to the arena.
    ///
    /// - `body` is the type of the response body.
    /// - `headers` is the type of the headers.
    /// - `content_type` should be a [`ValueId`](value::ValueId) pointing to a string defining the
    ///   content type.
    /// - `node` is the syntax node of the whole response.
    ///
    /// Returns the [`TypeId`] assigned to the response.
    pub fn add_response(
        &mut self,
        node: &SyntaxNode,
        body: Option<TypeId>,
        headers: Option<TypeId>,
        content_type: Option<value::ValueId>,
    ) -> TypeId {
        let idx = self.data.len();
        self.data.push(Tracked::new(
            node,
            Slot::Response {
                body,
                headers,
                content_type,
            },
        ));
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

    /// Add array to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested array.
    pub fn start_array<'a>(&'a mut self, node: &SyntaxNode) -> OptionArrayBuilder<'s, 'a> {
        OptionArrayBuilder::new_array(self, node)
    }

    /// Add option to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested option.
    pub fn start_option<'a>(&'a mut self, node: &SyntaxNode) -> OptionArrayBuilder<'s, 'a> {
        OptionArrayBuilder::new_option(self, node)
    }
}

impl<'s> BuilderParent<'s> for TypeArena<'s> {
    fn arena(&mut self) -> &mut TypeArena<'s> {
        self
    }
}

#[cfg(test)]
mod test {
    // use better_api_syntax::{ast::AstNode, parse, tokenize};
    //
    // use crate::typ::TypeArena;

    // #[test]
    // fn asdf() {
    //     let mut diags = vec![];
    //     let tokens = tokenize("", &mut diags);
    //     let res = parse(tokens);
    //
    //     let node = res.root.syntax();
    //
    //     let mut arena = TypeArena::new();
    //     let mut r1 = arena.start_record(node);
    // }
}
