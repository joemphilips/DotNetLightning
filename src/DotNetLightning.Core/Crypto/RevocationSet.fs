namespace DotNetLightning.Crypto

open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils

type InsertRevocationKeyError =
    | UnexpectedCommitmentNumber of got: CommitmentNumber * expected: CommitmentNumber
    | KeyMismatch of previousCommitmentNumber: CommitmentNumber * newCommitmentNumber: CommitmentNumber
    with
        member this.Message: string =
            match this with
            | UnexpectedCommitmentNumber(got, expected) ->
                sprintf
                    "Unexpected commitment number. Got %s, expected %s"
                    (got.ToString())
                    (expected.ToString())
            | KeyMismatch(previousCommitmentNumber, newCommitmentNumber) ->
                sprintf
                    "Revocation key for commitment %s derives a key for commitment %s which does \
                    not match the recorded key"
                    (newCommitmentNumber.ToString())
                    (previousCommitmentNumber.ToString())

type RevocationSet private (keys: list<CommitmentNumber * RevocationKey>) =
    new() = RevocationSet(List.empty)

    member this.Keys = keys

    static member FromKeys (keys: list<CommitmentNumber * RevocationKey>): RevocationSet =
        let rec sanityCheck (commitmentNumbers: list<CommitmentNumber>): bool =
            if commitmentNumbers.IsEmpty then
                true
            else
                let commitmentNumber = commitmentNumbers.Head
                let tail = commitmentNumbers.Tail
                match commitmentNumber.PreviousUnsubsumed with
                | None -> tail.IsEmpty
                | Some expectedCommitmentNumber ->
                    if tail.IsEmpty then
                        false
                    else
                        let nextCommitmentNumber = tail.Head
                        if nextCommitmentNumber <> expectedCommitmentNumber then
                            false
                        else
                            sanityCheck tail
        let commitmentNumbers, _ = List.unzip keys
        if not (sanityCheck commitmentNumbers) then
            failwith "commitment number list is malformed: %A" commitmentNumbers
        RevocationSet keys

    member this.NextCommitmentNumber: CommitmentNumber =
        if this.Keys.IsEmpty then
            CommitmentNumber.FirstCommitment
        else
            let prevCommitmentNumber, _ = this.Keys.Head
            prevCommitmentNumber.NextCommitment

    member this.InsertRevocationKey (commitmentNumber: CommitmentNumber)
                                    (revocationKey: RevocationKey)
                                        : Result<RevocationSet, InsertRevocationKeyError> =
        let nextCommitmentNumber = this.NextCommitmentNumber
        if commitmentNumber <> nextCommitmentNumber then
            Error <| UnexpectedCommitmentNumber (commitmentNumber, nextCommitmentNumber)
        else
            let rec fold (keys: list<CommitmentNumber * RevocationKey>)
                             : Result<RevocationSet, InsertRevocationKeyError> =
                if keys.IsEmpty then
                    let res = [commitmentNumber, revocationKey]
                    Ok <| RevocationSet res
                else
                    let storedCommitmentNumber, storedRevocationKey = keys.Head
                    match revocationKey.DeriveChild commitmentNumber storedCommitmentNumber with
                    | Some derivedRevocationKey ->
                        if derivedRevocationKey <> storedRevocationKey then
                            Error <| KeyMismatch (storedCommitmentNumber, commitmentNumber)
                        else
                            fold keys.Tail
                    | None ->
                        let res = (commitmentNumber, revocationKey) :: keys
                        Ok <| RevocationSet res
            fold this.Keys

    member this.GetRevocationKey (commitmentNumber: CommitmentNumber)
                                     : Option<RevocationKey> =
        let rec fold (keys: list<CommitmentNumber * RevocationKey>) =
            if keys.IsEmpty then
                None
            else
                let storedCommitmentNumber, storedRevocationKey = keys.Head
                match storedRevocationKey.DeriveChild storedCommitmentNumber commitmentNumber with
                | Some revocationKey -> Some revocationKey
                | None -> fold keys.Tail
        fold this.Keys

