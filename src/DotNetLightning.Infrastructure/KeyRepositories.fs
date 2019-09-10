namespace DotNetLightning.Infrastructure

open DotNetLightning.Chain
open NBitcoin
open DotNetLightning.Utils

type SupportedKeyRepositoryTypes =
    /// Only for testing
    | InMemory
    | FlatFile of path: string

type FlatFileKeyRepository() =
    interface IKeysRepository with
        member this.GenerateKeyFromBasePointAndSign(psbt: PSBT, pubkey: PubKey, basePoint: PubKey): TransactionSignature * PSBT = 
            failwith "Not Implemented"
        member this.GenerateKeyFromRemoteSecretAndSign(psbt: PSBT, pubKey: PubKey, remoteSecret: Key): TransactionSignature * PSBT = 
            failwith "Not Implemented"
        member this.GetChannelKeys(inbound: bool): ChannelKeys = 
            failwith "Not Implemented"
        member this.GetDestinationScript(): Script = 
            failwith "Not Implemented"
        member this.GetNodeSecret(): Key = 
            failwith "Not Implemented"
        member this.GetSessionKey(): Key = 
            failwith "Not Implemented"
        member this.GetShutdownPubKey(): PubKey = 
            failwith "Not Implemented"
        member this.GetSignatureFor(psbt: PSBT, pubKey: PubKey): TransactionSignature * PSBT = 
            failwith "Not Implemented"
