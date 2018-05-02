# Use case within EncryScript

    ---Schema---
    
    type Person(
        field name: String;
        field email: String;
    )
    
    # Structure of the box containing public key and creadentials of its owner,
    # can be used as a part of the dPKI protocol.
    type PublicKeyBox(
        field owner: Person;
        field pubKeyBytes: ByteVector;
    )
    
    ---Script---

    ...
