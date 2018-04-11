# EncryScript

[![Build Status](https://travis-ci.org/oskin1/EncryScript.svg?branch=master)](https://travis-ci.org/oskin1/EncryScript)

Statically-typed contract-oriented scripting language for [EncryCore](https://github.com/oskin1/EncryCore) protocol.

* [Language specification](https://github.com/oskin1/EncryScript/blob/master/docs/LanguageSpec.md)

EncryScript supports:
* Binary operations
* Boolean operations
* Accessing fields of the predefined data structures via dot-notation.
* Control-flow (`if`, `match`)
* Collections (`List[T]`, `Dict[T1, T2]`)
* Collection subscription by index (`Dict[T1, T2]` also supports by-key subscription)
* Constants definition
* Functions definition
* Lambdas definition

EncryScript does not support:
* `while` and `for` loops
* Recursive calls
* Nested collections

## Available data types

    // Primitives
    * Unit
    * Bool
    * String
    * Bytes
    * Long
    * Int
    * Float
    * Double
    
    // Collections
    * Dict[T0, T1]
    * List[T]
    
    * Option[T]
    
    // Built-in complex types
    * Transaction
    * Proof
    * Proposition
    * Context
    * Box
    * Signature25519
    * AssetBox
    * AccountProposition
    
## Contract examples

PublicKey lock:
        
    let ownerPubKey = base58"7AQchgGVqBzgCbngQK1tt9ZuLEbtTVQ2Ciq5evZtCaGq"
    match context.proof:
        case sig -> Signature25519:
            unlock if checkSig(sig.sigBytes, context.transaction.messageToSign, ownerPubKey)
        case _:
            abort
            
Time-window lock:

    let unlockedFrom: Long = unixTime('16-00-00:22-12-2018')
    let unlockedUntil: Long = unixTime('16-00-00:25-12-2018')
    unlock if context.state.lastBlockTimestamp >= unlockedFrom && ctx.lastBlockTimestamp <= unlockedUntil
        
State height lock:

    let unlockedFrom: Int = 100000
    unlock if context.state.height >= unlockedFrom

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).