# Encry Type Language specification

Type schema layout:

    type <TypeName>(
        field <Field1Name>: <Field1Type>;
        field <Field2Name>: <Field2Type>;
        ...
    )

Type schema example:

    type Customer(
        field name: String;
        field phone: String
        field lastVisit: Long
    )

Available primitive types:

    String
    Int
    Long
    Bool
    ByteVector
    List[T]
