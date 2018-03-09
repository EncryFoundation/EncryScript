# EncryScript Specification

## Available data types
    * Boolean
    * String
    * Byte
    * Bytes
    * Long
    * Int
    * Dict
    * List
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
    
    checkProofType(proof: Bytes, type: Byte): Boolean
    checkSig(msg: Bytes, sig: Bytes, pk: Bytes): Boolean
    pkFromAddress(addr: String): Bytes
    unixTime(ts: String): Long

## Use cases
Threshold signature (2 of 3):

    contract:
    
        publicKeys = {'Ivan' : pkFromAddress('5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'), 
                      'Marina' : pkFromAddress('11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'), 
                      'Alice': pkFromAddress('75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p')}
    
        if checkProofType(proof, MultiProof) and proof.proofs.size >= 2:
            sigFlag1 = if proof.proofs['Ivan'].isDefined && checkSig(ctx.transaction.bytes, proof.proofs['Ivan']!, publicKeys[0]) 1 else 0
            sigFlag2 = if proof.proofs['Ivan'].isDefined && checkSig(ctx.transaction.bytes, proof.proofs['Marina']!, publicKeys[1]) 1 else 0
            sigFlag3 = if proof.proofs['Ivan'].isDefined && checkSig(ctx.transaction.bytes, proof.proofs['Alice']!, publicKeys[2]) 1 else 0
            
            return (sigFlag1 + sigFlag2 + sigFlag3) >= 2
        return false
        
Time-window lock:

    contract:
    
        unlockedFrom = unixTime('16-00-00:22-12-2018')
        unlockedUntil = unixTime('16-00-00:25-12-2018')
        
        if ctx.networkTime >= unlockedFrom && ctx.networkTime <= unlockedUntil:
            return true
        return false
        
State height lock:

    contract:
    
        unlockedFrom = 100000
        
        if ctx.state.height >= unlockedFrom:
            return true
        return false
