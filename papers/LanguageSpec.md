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
    
    checkType(proof: bytes, type: any): bool
    checkSig(msg: bytes, sig: bytes, pk: bytes): bool
    pkFromAddress(addr: string): bytes
    unixTime(ts: string): long

## Type matching

    proof match:
        case sig -> Signature25519:
            unlock if checkSig()

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

    let unlockedFrom: long = unixTime('16-00-00:22-12-2018')
    let unlockedUntil: long = unixTime('16-00-00:25-12-2018')
    
    unlock if ctx.networkTime >= unlockedFrom && ctx.networkTime <= unlockedUntil
        
State height lock:

    let unlockedFrom: int = 100000
    
    unlock if ctx.state.height >= unlockedFrom
