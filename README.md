# EncryScript

[![Build Status](https://travis-ci.org/oskin1/EncryScript.svg?branch=master)](https://travis-ci.org/oskin1/EncryScript)

Statically-typed contract-oriented scripting language for EncryCore protocol.

EncryScript supports:
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

    let unlockedFrom: int = 100000
    unlock if context.state.height >= unlockedFrom

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).