// macro_rules! well_known_types_enum {
//     ($v:vis enum $enum_name:ident {
//         $($variant:ident = $expr:expr),*
//     }) => {
//         #[repr(usize)]
//         #[allow(non_camel_case_types)]
//         $v enum $enum_name {
//             $($variant,)*
//         }
//
//         impl $enum_name {
//             pub fn new_cache() -> HashMap<TypeId, TypeInfo> {
//                 let mut cache = HashMap::<TypeId, TypeInfo>::new();
//                 $(
//                     cache.insert(TypeTable::$variant, $expr);
//                 )*
//                 cache
//             }
//         }
//
//         impl TypeTable {
//             $(
//                 pub const $variant: TypeId = TypeId::from_u32($enum_name::$variant as u32);
//             )*
//         }
//     };
// }
//
// well_known_types_enum! {
//     pub enum WellKnownTypes {
//         TYPE_I8 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::I8}),
//         TYPE_I16 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::I16}),
//         TYPE_I32 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::I32}),
//         TYPE_I64 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::I64}),
//         TYPE_U8 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::U8}),
//         TYPE_U16 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::U16}),
//         TYPE_U32 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::U32}),
//         TYPE_U64 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::U64}),
//         TYPE_F32 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::F32}),
//         TYPE_F64 = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::F64}),
//         TYPE_BOOL = TypeInfo::Primitive(PrimitiveTypeInfo { primitive: ast::PrimitiveType::Bool}),
//         TYPE_STRING = TypeInfo::String,
//         TYPE_UNRESOLVED = TypeInfo::PartiallyResolved,
//         TYPE_UNIT = TypeInfo::Unit,
//         TYPE_NEVER = TypeInfo::Never,
//         TYPE_UNKNOWN = TypeInfo::Unknown,
//         TYPE_INFO_STRUCT = TypeInfo::Struct(StructTypeInfo {
//             name: SymbolName::root().child("TypeInfo"),
//             fields: vec![
//                 StructFieldInfo {
//                     name: ustr::ustr("size"),
//                     ty: TypeTable::TYPE_U64,
//                     has_default: false,
//                 },
//                 StructFieldInfo {
//                     name: ustr::ustr("align"),
//                     ty: TypeTable::TYPE_U64,
//                     has_default: false,
//                 },
//                 StructFieldInfo {
//                     name: ustr::ustr("type_id"),
//                     ty: TypeTable::TYPE_U64,
//                     has_default: false,
//                 },
//             ],
//         })
//     }
// }
//
//
