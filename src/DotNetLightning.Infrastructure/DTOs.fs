namespace DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.Channel

module DTO =
    open System.Text.Json
    open DotNetLightning.Serialize.Msgs
    open NBitcoin

    let inline private serializeWithName (name: string, data) =
        failwith ""

    module internal ChannelState =

        type InitFundeeDTO =
            {
                TemporaryChannelId: ChannelId
                LocalParams: LocalParams
                RemoteInit: Init
                ToLocal: LNMoney
            }
            with
            static member FromDomainObject(d: InputInitFundee) =
                {
                    TemporaryChannelId = d.TemporaryChannelId
                    LocalParams = d.LocalParams
                    RemoteInit = d.RemoteInit
                    ToLocal = d.ToLocal
                }
        type ChannelStateDTO =
            | WaitForInitInternal
            | WaitForOpenChannelDTO of InitFundeeDTO
            | WaitForAcceptChannelDTO of WaitForAcceptChannelData
            | WaitForFundingCreatedDTO of WaitForFundingCreatedData
            | WaitForFundingSignedDTO of WaitForFundingSignedData
            | WaitForFundingConfirmedDTO of WaitForFundingConfirmedData
            | WaitForFundingLockedDTO of WaitForFundingLockedData

            /// normal
            | NormalDTO of NormalData

            /// Closing
            | ShutdownDTO of ShutdownData
            | NegotiatingDTO of NegotiatingData
            | ClosingDTO of ClosingData
            | ClosedDTO of IChannelStateData

            /// Abnormal
            | OfflineDTO of IChannelStateData
            | SyncingDTO of IChannelStateData
            | WaitForRemotePublishFutureCommitmentDTO of WaitForRemotePublishFutureCommitmentData

            | ErrFundingTimeOutDTO of IChannelStateData
            | ErrInformationLeakDTO of IChannelStateData
            with
            static member FromDomainObject(s: ChannelState) =
                match s with
                | WaitForOpenChannel d ->
                    WaitForOpenChannelDTO (InitFundeeDTO.FromDomainObject(d.InitFundee))

            member this.ToDomainObject(): RResult<_> =
                match this with
                | WaitForOpenChannelDTO dto ->
                    { WaitForOpenChannelData.InitFundee = { InputInitFundee.TemporaryChannelId = dto.TemporaryChannelId
                                                            LocalParams = failwith "Not Implemented"
                                                            RemoteInit = failwith "Not Implemented"
                                                            ToLocal = failwith "Not Implemented"
                                                            ChannelKeys = failwith "Not Implemented" } }
                    |> ChannelState.WaitForOpenChannel
                    |> Good

            static member Deserialize(json: string) =
                use jsonDocument = JsonDocument.Parse(json)
                let root = jsonDocument.RootElement.[0]
                failwith "not implemented"

    [<CLIMutable>]
    type internal ChannelDTO = {
        RemoteNodeId: NodeId
        Config: ChannelConfig
        LocalNodeSecret: Key
        State: ChannelState.ChannelStateDTO
    }
        with
        static member FromDomainObject (c: Channel) =
            {
                ChannelDTO.RemoteNodeId = c.RemoteNodeId
                Config = c.Config
                LocalNodeSecret = c.LocalNodeSecret
                State = ChannelState.ChannelStateDTO.FromDomainObject(c.State)
            }
        
        member this.ToDomainObject(): RResult<_> =
            failwith ""

        static member Deserialize(txt: string): RResult<ChannelDTO> =
            try
                JsonSerializer.Deserialize<ChannelDTO>(txt)
                |> Good
            with
                | ex -> RResult.rexn ex

        member this.Serialize() =
            JsonSerializer.Serialize<ChannelDTO>(this)
