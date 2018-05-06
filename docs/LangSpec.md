# Encry Type Language specification

Type schema layout:

    schema <SchemaName>:<SchemaType>

Type schema example:

    schema CustomerBox:Object(
        person:Object(name:String; age:Int);
        history:Object(ltv:Long; bonus:Int);
        orders:Array[Object(product_id:Long; amount:Long;)];
        id:Long;
    )
    
    CustomerBox({
        "person":{"name":"Julia","age":23},
        "history":{"ltv":23010,"bonus":198},
        "orders":[{"producr_id":19273421,"amount":240},{"producr_id":69127344,"amount":126}],
        "id":7564382569
    })

Available primitive types:

    String
    Int
    Long
    Bool
    ByteVector
    List[T]
    Option[T]
