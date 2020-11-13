namespace DotNetLightning.Crypto

open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

type InsertPerCommitmentSecretError =
    | UnexpectedCommitmentNumber of got: CommitmentNumber * expected: CommitmentNumber
    | SecretMismatch of previousCommitmentNumber: CommitmentNumber * newCommitmentNumber: CommitmentNumber
    with
        member this.Message: string =
            match this with
            | UnexpectedCommitmentNumber(got, expected) ->
                sprintf
                    "Unexpected commitment number. Got %s, expected %s"
                    (got.ToString())
                    (expected.ToString())
            | SecretMismatch(previousCommitmentNumber, newCommitmentNumber) ->
                sprintf
                    "Per commitment secret for commitment %s derives a secret \
                    for commitment %s which does not match the recorded secret"
                    (newCommitmentNumber.ToString())
                    (previousCommitmentNumber.ToString())

type PerCommitmentSecretStore private (secrets: list<CommitmentNumber * PerCommitmentSecret>) =
    new() = PerCommitmentSecretStore(List.empty)

    member this.Secrets = secrets

    static member FromSecrets (secrets: list<CommitmentNumber * PerCommitmentSecret>): PerCommitmentSecretStore =
        let rec sanityCheck (commitmentNumbers: list<CommitmentNumber>): bool =
            if commitmentNumbers.IsEmpty then
                true
            else
                let commitmentNumber = commitmentNumbers.Head
                let tail = commitmentNumbers.Tail
                match commitmentNumber.PreviousUnsubsumed() with
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
        let commitmentNumbers, _ = List.unzip secrets
        if not (sanityCheck commitmentNumbers) then
            failwithf "commitment number list is malformed: %A" commitmentNumbers
        PerCommitmentSecretStore secrets

    member this.NextCommitmentNumber(): CommitmentNumber =
        if this.Secrets.IsEmpty then
            CommitmentNumber.FirstCommitment
        else
            let prevCommitmentNumber, _ = this.Secrets.Head
            prevCommitmentNumber.NextCommitment()

    member this.InsertPerCommitmentSecret (commitmentNumber: CommitmentNumber)
                                          (perCommitmentSecret: PerCommitmentSecret)
                                              : Result<PerCommitmentSecretStore, InsertPerCommitmentSecretError> =
        let nextCommitmentNumber = this.NextCommitmentNumber()
        if commitmentNumber <> nextCommitmentNumber then
            Error <| UnexpectedCommitmentNumber (commitmentNumber, nextCommitmentNumber)
        else
            let rec fold (secrets: list<CommitmentNumber * PerCommitmentSecret>)
                             : Result<PerCommitmentSecretStore, InsertPerCommitmentSecretError> =
                if secrets.IsEmpty then
                    let res = [commitmentNumber, perCommitmentSecret]
                    Ok <| PerCommitmentSecretStore res
                else
                    let storedCommitmentNumber, storedPerCommitmentSecret = secrets.Head
                    match perCommitmentSecret.DeriveChild commitmentNumber storedCommitmentNumber with
                    | Some derivedPerCommitmentSecret ->
                        if derivedPerCommitmentSecret <> storedPerCommitmentSecret then
                            Error <| SecretMismatch (storedCommitmentNumber, commitmentNumber)
                        else
                            fold secrets.Tail
                    | None ->
                        let res = (commitmentNumber, perCommitmentSecret) :: secrets
                        Ok <| PerCommitmentSecretStore res
            fold this.Secrets

    member this.GetPerCommitmentSecret (commitmentNumber: CommitmentNumber)
                                           : Option<PerCommitmentSecret> =
        let rec fold (secrets: list<CommitmentNumber * PerCommitmentSecret>) =
            if secrets.IsEmpty then
                None
            else
                let storedCommitmentNumber, storedPerCommitmentSecret = secrets.Head
                match storedPerCommitmentSecret.DeriveChild storedCommitmentNumber commitmentNumber with
                | Some perCommitmentSecret -> Some perCommitmentSecret
                | None -> fold secrets.Tail
        fold this.Secrets

    member this.MostRecentPerCommitmentSecret(): Option<PerCommitmentSecret> =
        if this.Secrets.IsEmpty then
            None
        else
            let _, perCommitmentSecret = this.Secrets.Head
            Some perCommitmentSecret

