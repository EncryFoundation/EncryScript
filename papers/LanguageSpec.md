# EncryScript Specification

## Available data types
    * String
    * Byte
    * Bytes
    * Long
    * Int
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
    
    checkProofType(proof, type): bool
    checkSig(msg, sig, pk): bool
    pkFromAddress(addr): Bytes

## Use cases
Threshold signature (2 of 3)

    contract:
    
        publicKeys = {'Ivan' : pkFromAddress('5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'), 
                      'Marina' : pkFromAddress('11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'), 
                      'Alice': pkFromAddress('75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p')}
    
        if checkProofType(proof, MultiProof) and proof.proofs.size >= 2:
            sigFlag1 = if checkSig(ctx.transaction.bytes, proof.proofs['Ivan']?.sig, publicKeys[0]) 1 else 0
            sigFlag2 = if checkSig(ctx.transaction.bytes, proof.proofs['Marina']?.sig, publicKeys[1]) 1 else 0
            sigFlag3 = if checkSig(ctx.transaction.bytes, proof.proofs['Alice']?.sig, publicKeys[2]) 1 else 0
            
            return (sigFlag1 + sigFlag2 + sigFlag3) >= 2
        return false
        