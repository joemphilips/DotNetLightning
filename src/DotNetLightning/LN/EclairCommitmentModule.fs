namespace DotNetLightning.LN

open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Serialize.Msgs

module Commitments =
    let addLocalProposal (proposal: IUpdateMsg) (c: Commitments) =
        let lens = Commitments.LocalChanges_ >-> LocalChanges.Proposed_
        Optic.map lens (fun proposalList -> proposal :: proposalList) c

    let 