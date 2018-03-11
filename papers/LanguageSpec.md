# EncryScript Specification

* Strong-typed
* Statically-typed

Language abilities:
* Binary operations
* Accessing fields of the predefined data structures via dot-notation.
* Control-flow (if, for .. in)
* Variable (? Constants) definition
* ? Functions definition (Prohibit recursive calls?) 

## Available data types

    * any
    * bool
    * str
    * byte
    * bytes
    * long
    * int
    * dict
    * list
    * <type>?               // Option[<type>]

## Predefined data structures

    * Context
        
        Transaction
            senderPubKey
            timestamp
            id
        
        State
            height
            networkTime
            
    * Proof
        typeId
        body                    // SigBytes in case of Signature25519
        
    * Reference
        Signature25519           // Object
        MultiProof               // TODO: Add to EncryCore protocol
       
## Built-in functions
    
    checkType(proof: Bytes, type: Any): Boolean
    checkSig(msg: Bytes, sig: Bytes, pk: Bytes): Boolean
    pkFromAddress(addr: String): Bytes
    unixTime(ts: String): Long

## Use cases
Threshold signature (2 of 3):
    
    let publicKeys = {'Ivan' : pkFromAddress('5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'), 
                      'Marina' : pkFromAddress('11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'), 
                      'Alice': pkFromAddress('75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p')}

    if checkType(proof, MultiProof) and proof.proofs.size >= 2:
        sigFlag1 = 1 if proof.proofs['Ivan'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Ivan']!, publicKeys[0])) else 0
        sigFlag2 = 1 if proof.proofs['Ivan'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Marina']!, publicKeys[1]) else 0
        sigFlag3 = 1 if proof.proofs['Ivan'].isDefined and checkSig(ctx.transaction.bytes, proof.proofs['Alice']!, publicKeys[2]) else 0
        
        if (sigFlag1 + sigFlag2 + sigFlag3) >= 2:
            unlock
        
Time-window lock:

    unlockedFrom: long = unixTime('16-00-00:22-12-2018')
    unlockedUntil: long = unixTime('16-00-00:25-12-2018')
    
    if ctx.networkTime >= unlockedFrom && ctx.networkTime <= unlockedUntil:
        unlock
        
State height lock:

    let unlockedFrom: int = 100000
    
    if ctx.state.height >= unlockedFrom:
        unlock
