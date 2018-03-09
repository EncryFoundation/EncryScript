# EncryScript Specification

## Available data types
    * String
    * Bytes
    * Long
    

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

## Use cases
Threshold signature (2 of 3)

    contract:
        if checkProofType(proof, MultiProof) and proof.proofs.size >= 2:
            sigFlag1 = if checkSig(ctx.transaction.bytes, proof.proofs[0].sig, proof.proofs[0].pk) 1 else 0
            sigFlag2 = if checkSig(ctx.transaction.bytes, proof.proofs[1].sig, proof.proofs[1].pk) 1 else 0
            sigFlag3 = if checkSig(ctx.transaction.bytes, proof.proofs[2].sig, proof.proofs[2].pk) 1 else 0
            
            return (sigFlag1 + sigFlag2 + sigFlag3) >= 2
        return false
        