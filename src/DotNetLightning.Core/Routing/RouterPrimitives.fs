namespace DotNetLightning.Routing

open DotNetLightning.Payment
open System
open NBitcoin
open ResultUtils
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open Graph

[<AutoOpen>]
module RouterPrimitives =
    let checkUpdate(x: ChannelUpdate option, msg ) =
        match x with
        | Some x -> Result.requireTrue msg (x.IsNode1)
        | None -> Ok()
        
    let isNode1(localNodeId: NodeId, remoteNodeId: NodeId) =
        localNodeId > remoteNodeId
            
    type RouterConf = {
        RandomizeRouterSelection: bool
        ChannelExcludeDuration: TimeSpan
        RouterBroadcastInterval: TimeSpan
        SearchMaxFeeBase: LNMoney
        SearchMaxFeePct: double
        SearchMaxRouteLength: int
        SearchMaxCLTV: BlockHeightOffset
        SearchHeuristicsEnabled: bool
        SearchRatioCLTV: double
        SearchRatioChannelAge: double
        SearchRatioChannelCapacity: double
    }

    type PublicChannel = private {
        Announce: ChannelAnnouncement
        FundingTxId: TxId
        Cap: Money
        MaybeUpdate1: ChannelUpdate option
        MaybeUpdate2: ChannelUpdate option
    }
        with
        static member TryCreate (a, f, c, ?update1: ChannelUpdate, ?update2: ChannelUpdate) =
            result {
                do! checkUpdate(update1, "Update 1 must be node 1 according to bolt 7 definition")
                do! checkUpdate(update2, "Update 2 must be node 2 according to bolt 7 definition")
                return { Announce = a; FundingTxId = f; Cap = c; MaybeUpdate1 = update1; MaybeUpdate2 = update2 }
            }

        member this.GetNodeIdSameSideAs(u: ChannelUpdate) =
            if (u.IsNode1) then this.Announce.Contents.NodeId1 else this.Announce.Contents.NodeId2
            
        member this.GetChannelUpdateSameSideAs(u: ChannelUpdate) =
            if (u.IsNode1) then this.MaybeUpdate1 else this.MaybeUpdate2
            
        member this.UpdateChannelUpdateSameSideAs(u: ChannelUpdate) =
            if (u.IsNode1) then { this with MaybeUpdate1 = Some u } else { this with MaybeUpdate2 = Some u }
            
        member this.GetChannelUpdateField<'T> (f: ChannelUpdate -> 'T): seq<'T> =
            seq {
                yield! this.MaybeUpdate1 |> Option.toList
                yield! this.MaybeUpdate2 |> Option.toList
            } |> Seq.map f
            
    type PrivateChannel = {
        LocalNodeId: NodeId
        RemoteNodeId: NodeId
        MaybeUpdate1: ChannelUpdate option
        MaybeUpdate2: ChannelUpdate option
    }

        with
        member this.GetNodeIdSameSideAs(u: ChannelUpdate) =
            let node1, node2 =
                if (isNode1(this.LocalNodeId, this.RemoteNodeId)) then
                    (this.LocalNodeId, this.RemoteNodeId)
                else (this.RemoteNodeId, this.LocalNodeId)
            if (u.IsNode1) then node1 else node2
            
        member this.GetChannelUpdateSameSideAs(u: ChannelUpdate) =
            if (u.IsNode1) then this.MaybeUpdate1 else this.MaybeUpdate2
        member this.UpdateChannelUpdateSameSideAs(u: ChannelUpdate) =
            if (u.IsNode1) then { this with MaybeUpdate1 = Some u } else { this with MaybeUpdate2 = Some u }
            
    type AssistedChannel = {
        ExtraHop: ExtraHop
        NextNodeId: NodeId
        HTLCMaximum: LNMoney
    }
    type IHop =
        abstract member NodeId: NodeId
        abstract member NextNodeId: NodeId
        abstract member CLTVExpiryDelta: BlockHeightOffset
        abstract member Fee: amount: LNMoney -> LNMoney
        
    /// A directed hop between two connected nodes using a specific channel.
    type ChannelHop = private {
        /// The id of the start node
        NodeId: NodeId
        /// The id of the end node
        NextNodeId: NodeId
        CLTVExpiryDelta: BlockHeightOffset
        LastUpdate: UnsignedChannelUpdate
    }
        with
        member this.LastUpdateValue = this.LastUpdate
        member this.NodeIdValue = this.NodeId
        member this.NextNodeIdValue = this.NextNodeId
        static member Create (a, b, u: UnsignedChannelUpdate) =
            {
                NodeId = a
                NextNodeId = b
                CLTVExpiryDelta = u.CLTVExpiryDelta
                LastUpdate = u
            }
        static member FromGraphEdge(g: GraphLabel) =
            ChannelHop.Create(g.Desc.A, g.Desc.B, g.Update)
        interface IHop with
            member this.NodeId = this.NodeId
            member this.NextNodeId = this.NextNodeId
            member this.CLTVExpiryDelta = this.CLTVExpiryDelta
            member this.Fee amount =
                let u = this.LastUpdate
                (u.FeeBaseMSat.MilliSatoshi + (int64 u.FeeProportionalMillionths * amount.MilliSatoshi) / 1000000L)
                |> LNMoney.MilliSatoshis
            
