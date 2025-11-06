use better_api_syntax::ast;
use string_interner::DefaultStringInterner;

use crate::Element;
use crate::oracle::value::insert_array_values;
use crate::typ::{PrimitiveType, TypeId};
use crate::value::Value;

use super::Oracle;

impl Oracle {
    pub(crate) fn parse_type(&mut self, typ: &ast::Type) -> TypeId {
        let id = match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => self.types.add_primitive(primitive),
            ParsedType::Enum(en) => self.parse_enum(en),
            ParsedType::Response(type_response) => todo!(),
            ParsedType::Record(record) => todo!(),
            ParsedType::Union(union) => todo!(),
            ParsedType::Array(type_array) => todo!(),
            ParsedType::Option(type_option) => todo!(),
        };

        self.source_map.insert(typ, Element::Type(id));
        id
    }

    /// Parses enum type and inserts it into arena
    fn parse_enum(&mut self, typ: &ast::Enum) -> TypeId {
        // TODO: Validate enum type and members types

        // Parse type of the enum
        let enum_type_id = typ.typ().map(|t| self.parse_type(&t));

        // Parse enum members
        let builder = self.values.start_array();
        let members_id = insert_array_values(
            typ.members().filter_map(|m| m.value()),
            builder,
            &mut self.source_map,
            &mut self.reports,
            &mut self.strings,
        );

        // Insert members into source map
        let arr = match self.values.get(members_id) {
            Value::Array(arr) => arr,
            _ => unreachable!("inserted array should be an array"),
        };
        for (member, arr_item) in typ.members().filter(|m| m.value().is_some()).zip(arr) {
            self.source_map
                .insert(&member, Element::EnumMember(arr_item.id));
        }

        self.types.add_enum(enum_type_id, members_id)
    }
}

enum ParsedType<'a> {
    Primitive(PrimitiveType),
    Enum(&'a ast::Enum),
    Response(&'a ast::TypeResponse),
    Record(&'a ast::Record),
    Union(&'a ast::Union),
    Array(&'a ast::TypeArray),
    Option(&'a ast::TypeOption),
}

impl<'a> ParsedType<'a> {
    fn new(typ: &'a ast::Type, strings: &mut DefaultStringInterner) -> Self {
        match typ {
            ast::Type::TypeOption(opt) => Self::Option(opt),
            ast::Type::TypeArray(arr) => Self::Array(arr),
            ast::Type::Record(rec) => Self::Record(rec),
            ast::Type::Enum(en) => Self::Enum(en),
            ast::Type::Union(union) => Self::Union(union),
            ast::Type::TypeResponse(resp) => Self::Response(resp),
            ast::Type::TypeRef(typ_ref) => {
                let token = typ_ref.name();
                let str_id = strings.get_or_intern(token.text());

                Self::Primitive(PrimitiveType::Reference(str_id))
            }
            ast::Type::TypeI32(_) => Self::Primitive(PrimitiveType::I32),
            ast::Type::TypeI64(_) => Self::Primitive(PrimitiveType::I64),
            ast::Type::TypeU32(_) => Self::Primitive(PrimitiveType::U32),
            ast::Type::TypeU64(_) => Self::Primitive(PrimitiveType::U64),
            ast::Type::TypeF32(_) => Self::Primitive(PrimitiveType::F32),
            ast::Type::TypeF64(_) => Self::Primitive(PrimitiveType::F64),
            ast::Type::TypeDate(_) => Self::Primitive(PrimitiveType::Date),
            ast::Type::TypeTimestamp(_) => Self::Primitive(PrimitiveType::Timestamp),
            ast::Type::TypeBool(_) => Self::Primitive(PrimitiveType::Bool),
            ast::Type::TypeString(_) => Self::Primitive(PrimitiveType::String),
            ast::Type::TypeFile(_) => Self::Primitive(PrimitiveType::File),
        }
    }
}
