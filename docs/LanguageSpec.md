# EncryScript Specification

* Strong-typed
* Statically-typed

Language abilities:
* Binary operations
* Boolean operations
* Accessing fields of the predefined data structures via dot-notation.
* Control-flow (if, match)
* Constants definition
* Functions definition (Recursive calls are prohibited)

## Available data types

    * Any
    * Bool
    * String
    * Bytes
    * Long
    * Int
    * Float
    * Double
    * Dict[T0, T1]
    * List[T]
    * Option[T]

## Predefined data structures

    * Context
        
        Transaction
            accountPubKey
            fee
            timestamp
            sig
            inputs:
                Unlocker
                    boxId
                    proofOpt:
                        Proof (Abstract type)
                        -> Signature25519
                        -> MultiProof
            outputs:
                Box (Abstract type)
                    proposition:
                        Proposition (Abstract type)
                            typeId
                        -> ContractProposition
                            script
                        -> AccountProposition
                            address
                -> AssetBox
                    amount
                -> GenericBox (?)
                    fields:
                        Seq[GBoxField]
                            GBoxField:
                                name
                                value
                                type
                                    GBoxFieldType (Abstract type)
                                    -> GBInt
                                    -> GBString
                                    -> GBByteVector

        State
            height
            lastBlockTimestamp
            stateDigest

        Proof
            typeId
            body                    // SigBytes in case of Signature25519
        
    * Reference
        Signature25519           // Object
        MultiProof               // TODO: Add to EncryCore protocol
       
## Built-in functions
    
    checkSig(msg: bytes, sig: bytes, pk: bytes): Bool
    pkFromAddress(addr: string): Bytes
    unixTime(ts: string): Long

    // Hash-functions
    blake2b256Hash(input: Bytes): Bytes
    blake2b512Hash(input: Bytes): Bytes
    keccak256Hash(input: Bytes): Bytes
    keccak512Hash(input: Bytes): Bytes
    sha256Hash(input: Bytes): Bytes

## Syntax

    // Constant definition
    let a: Int = 10                     // Explicit type declaration
    let b = 100                         // Type will be inferred automatically
    let c = true if a < b else false    // Conditional assignment
    global let d = "string"             // Global declaration

    // Function definition
    def sum(a: Int, b: Int) -> Int:
        return a + b

    // If statement
    if (10 < 100):
        pass
    elif (10 == 100):
        pass
    else:
        pass

    // Base58 string
    let pubKeyBytes: Bytes = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'

    // Collections
    let ageList: List[Int] = [1, 2, 3, 4]   // Note, types could also be inferred implicitly by preprecessor.
    let ageDict: Dict[String, Int] = {'Alice' : 4, 'Bob' : 9, 'Tom' : 17}

    let someonesAge = ageList[0].get        // Will result in `1`
    let aliceAge = ageDict['Alice'].get     // Will result in `4`

    // Lambdas application
    let deesExist: Bool = ageList.exists(lamb (i: Int) = i > 3)     // Will result in `true`

    // Matching
    match poof:
        case sig -> Signature25519:
            unlock if checkSig()        // Unlock condition
        case _:
            abort                       // Halt script execution

## Usage with EncryTL

    // EncryTL schema description
    schema PersonData:Object(
        name:String;
        age:Int;
    )

    #---script---

    def checkAge(box: Box) -> Bool:
       match box:
           case dataBox -> DataBox:
               match read(dataBox.data).get:
                   case person -> @PersonData:
                       return person.body.age > 20
                   case _:
                       return false
           case _:
               return false

## Use cases
Threshold signature (2 of 3):
    
    let publicKeys = {'Ivan' : pkFromAddress('5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'),
                      'Marina' : pkFromAddress('11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'),
                      'Alice': pkFromAddress('75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p')}

    if checkType(proof, MultiProof) and proof.proofs.size >= 2:
        let sigFlag1 = 1 if proof.proofs['Ivan'].isDefined and checkSig(ctx.transaction.txHash, proof.proofs['Ivan'].get, publicKeys[0])) else 0
        let sigFlag2 = 1 if proof.proofs['Marina'].isDefined and checkSig(ctx.transaction.txHash, proof.proofs['Marina'].get, publicKeys[1]) else 0
        let sigFlag3 = 1 if proof.proofs['Alice'].isDefined and checkSig(ctx.transaction.txHash, proof.proofs['Alice'].get, publicKeys[2]) else 0
        
        unlock if (sigFlag1 + sigFlag2 + sigFlag3) >= 2
        
Time-window lock:

    let unlockedFrom: Long = unixTime('16-00-00:22-12-2018')
    let unlockedUntil: Long = unixTime('16-00-00:25-12-2018')
    
    unlock if context.state.lastBlockTimestamp >= unlockedFrom && ctx.lastBlockTimestamp <= unlockedUntil
        
State height lock:

    let unlockedFrom: Int = 100000

    unlock if context.state.height >= unlockedFrom