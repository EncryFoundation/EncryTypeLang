# Encry Type Language specification

Type schema layout:

    schema <SchemaName>:<SchemaType>

Type schema example:

    schema CustomerBox:Object(
        person:Object(name:String; age:Int);
        orders:List[Object(product_id:Long; amount:Long;)];
        id:Long;
    )
    
Json equivalent of the example above:

    {
        "person" : {
            "type" : "Object",
            "value" : {
                "name" : {
                    "type" : "String",
                    "value" : "Julia"
                    },
                "age" : {
                    "type" : "Int",
                    "value" : 23
                    }
                }
            },
        "orders": {
            "type" : "List[Object]",
            "value" : [{
                "product_id" : {
                    "type" : "Long",
                    "value" : 19273421},
                "amount" : {
                    "type" : "Long",
                    "value" : 240
                    }
                },
                {
                "product_id" : {
                    "type" : "Long",
                    "value" : 19273421
                    },
                "amount" : {
                    "type" : "Long",
                    "value" : 240
                    }
                }]
            },
        "id" : {
            "type" : "Long",
            "value" : 7564382569
        }
    }

Available primitive types:

    Any
    Int
    Long
    String
    Bool
    ByteVector
    List[T]
