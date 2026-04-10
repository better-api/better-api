use crate::spec::Spec;
use crate::spec::arena::value::{
    ArrayCursor, ObjectCursor, ObjectFieldId, PrimitiveValue, ValueData, ValueId,
};
use crate::text::Name;

/// Representation of a value.
#[derive(Debug, derive_more::Display, Clone)]
pub enum ValueView<'a> {
    #[display("`null`")]
    Null,
    #[display("string")]
    String(&'a str),
    #[display("bool")]
    Bool(bool),
    #[display("integer")]
    Integer(i128), // i128 is large enough for i64 and u64
    #[display("float")]
    Float(f64),
    #[display("object")]
    Object(ObjectView<'a>),
    #[display("array")]
    Array(ArrayView<'a>),
}

impl<'a> ValueView<'a> {
    fn new(spec: &'a Spec, id: ValueId) -> Self {
        let data = spec.values.get_value(id);
        match data {
            ValueData::Primitive(PrimitiveValue::Null) => ValueView::Null,
            ValueData::Primitive(PrimitiveValue::Bool(b)) => ValueView::Bool(b),
            ValueData::Primitive(PrimitiveValue::Integer(i)) => ValueView::Integer(i),
            ValueData::Primitive(PrimitiveValue::Float(f)) => ValueView::Float(f),
            ValueData::Primitive(PrimitiveValue::String(id)) => {
                let string = spec.strings.resolve(id);
                ValueView::String(string)
            }
            ValueData::Array(array_range) => {
                let cursor = spec.values.array_cursor(array_range);
                Self::Array(ArrayView {
                    id,
                    items: ArrayItemIter { spec, cursor },
                })
            }
            ValueData::Object(object_range) => {
                let cursor = spec.values.object_cursor(object_range);
                Self::Object(ObjectView {
                    id,
                    fields: ObjectFieldIter { spec, cursor },
                })
            }
        }
    }
}

/// A field inside of the object.
#[derive(Debug, Clone)]
pub struct ObjectFieldView<'a> {
    #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
    pub(crate) id: ObjectFieldId,
    pub name: &'a Name,
    pub value: ValueView<'a>,
}

/// Object value stored in the spec value arena.
#[derive(Debug, Clone)]
pub struct ObjectView<'a> {
    // Id of the object
    #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
    pub(crate) id: ValueId,

    /// Iterator over fields.
    fields: ObjectFieldIter<'a>,
}

impl<'a> ObjectView<'a> {
    /// Returns iterator over [object fields](ObjectFieldView).
    pub fn fields(&self) -> ObjectFieldIter<'a> {
        self.fields.clone()
    }
}

/// Iterator over object fields.
#[derive(derive_more::Debug, Clone)]
pub struct ObjectFieldIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,

    cursor: ObjectCursor,
}

impl<'a> Iterator for ObjectFieldIter<'a> {
    type Item = ObjectFieldView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (field, next) = self.spec.values.next_object_field(self.cursor)?;

        let name = self.spec.strings.resolve_name(field.name);
        let value = ValueView::new(self.spec, field.value_id);

        self.cursor = next;

        Some(ObjectFieldView {
            id: field.id,
            name,
            value,
        })
    }
}

/// Item returned by [`ArrayView::items`].
#[derive(Debug)]
pub struct ArrayItemView<'a> {
    /// Id of the item in the value arena.
    #[cfg_attr(
        not(test),
        expect(dead_code, reason = "Should be used later on by semantic query APIs")
    )]
    pub(crate) id: ValueId,

    /// Value of the item
    pub value: ValueView<'a>,
}

/// Array value stored in the spec value arena.
#[derive(Debug, Clone)]
pub struct ArrayView<'a> {
    // Id of the array
    #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
    pub(crate) id: ValueId,

    items: ArrayItemIter<'a>,
}

impl<'a> ArrayView<'a> {
    /// Returns iterator over [array items](ArrayItemView)
    pub fn items(&self) -> ArrayItemIter<'a> {
        self.items.clone()
    }
}

/// Iterator over array items.
#[derive(derive_more::Debug, Clone)]
pub struct ArrayItemIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,

    cursor: ArrayCursor,
}

impl<'a> Iterator for ArrayItemIter<'a> {
    type Item = ArrayItemView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (id, next) = self.spec.values.next_array_item(self.cursor)?;

        let value = ValueView::new(self.spec, id);

        self.cursor = next;
        Some(ArrayItemView { id, value })
    }
}

impl Spec {
    pub(crate) fn get_value<'a>(&'a self, id: ValueId) -> ValueView<'a> {
        ValueView::new(self, id)
    }
}

#[cfg(test)]
mod test {
    use super::ValueView;
    use crate::spec::Spec;
    use crate::spec::arena::value::PrimitiveValue;
    use crate::text::NameId;

    #[test]
    fn iterates_object_fields_and_array_items() {
        let mut spec = Spec::new_test();

        let title_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("title")) };
        let list_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("list")) };
        let nested_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("nested")) };
        let ok_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("ok")) };
        let hello = spec.strings.get_or_intern("hello");

        let mut root = spec.values.start_object();
        root.add_primitive(title_name, PrimitiveValue::String(hello));

        let (mut list, _) = root.start_array(list_name);
        list.add_primitive(PrimitiveValue::Integer(1));
        list.add_primitive(PrimitiveValue::Integer(2));
        list.finish();

        let (mut nested, _) = root.start_object(nested_name);
        nested.add_primitive(ok_name, PrimitiveValue::Bool(true));
        nested.finish();

        let root_id = root.finish();

        let root = match ValueView::new(&spec, root_id) {
            ValueView::Object(object) => object,
            other => panic!("expected object at root, got {other:?}"),
        };

        let mut fields = root.fields();

        let title = fields.next().expect("title field");
        assert_eq!(title.name.as_str(), "title");
        assert!(matches!(title.value, ValueView::String("hello")));

        let list = fields.next().expect("list field");
        assert_eq!(list.name.as_str(), "list");
        let mut list_items = match list.value {
            ValueView::Array(array) => array.items(),
            other => panic!("expected array for list, got {other:?}"),
        };
        assert!(matches!(
            list_items.next().expect("first list item").value,
            ValueView::Integer(1)
        ));
        assert!(matches!(
            list_items.next().expect("second list item").value,
            ValueView::Integer(2)
        ));
        assert!(list_items.next().is_none());

        let nested = fields.next().expect("nested field");
        assert_eq!(nested.name.as_str(), "nested");
        let mut nested_fields = match nested.value {
            ValueView::Object(object) => object.fields(),
            other => panic!("expected object for nested, got {other:?}"),
        };
        let ok = nested_fields.next().expect("nested ok field");
        assert_eq!(ok.name.as_str(), "ok");
        assert!(matches!(ok.value, ValueView::Bool(true)));
        assert!(nested_fields.next().is_none());

        assert!(fields.next().is_none());
    }

    #[test]
    fn iterates_array_items_with_nested_values() {
        let mut spec = Spec::new_test();

        let flag_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("flag")) };

        let mut root = spec.values.start_array();
        root.add_primitive(PrimitiveValue::Integer(1));

        let mut object_item = root.start_object();
        object_item.add_primitive(flag_name, PrimitiveValue::Bool(false));
        object_item.finish();

        let mut array_item = root.start_array();
        array_item.add_primitive(PrimitiveValue::Integer(3));
        array_item.finish();

        let root_id = root.finish();

        let root = match ValueView::new(&spec, root_id) {
            ValueView::Array(array) => array,
            other => panic!("expected array at root, got {other:?}"),
        };

        let mut items = root.items();

        assert!(matches!(
            items.next().expect("first item").value,
            ValueView::Integer(1)
        ));

        let object_item = items.next().expect("second item");
        let mut object_fields = match object_item.value {
            ValueView::Object(object) => object.fields(),
            other => panic!("expected object as second item, got {other:?}"),
        };
        let flag = object_fields.next().expect("flag field");
        assert_eq!(flag.name.as_str(), "flag");
        assert!(matches!(flag.value, ValueView::Bool(false)));
        assert!(object_fields.next().is_none());

        let nested_array = items.next().expect("third item");
        let mut nested_items = match nested_array.value {
            ValueView::Array(array) => array.items(),
            other => panic!("expected array as third item, got {other:?}"),
        };
        assert!(matches!(
            nested_items.next().expect("nested first item").value,
            ValueView::Integer(3)
        ));
        assert!(nested_items.next().is_none());

        assert!(items.next().is_none());
    }
}
