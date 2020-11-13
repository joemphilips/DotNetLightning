module DotNetLightning.Tests.LSATTests

open System
open System.Linq
open Expecto
open DotNetLightning.Payment.LSAT
open Macaroons

open ResultUtils
open ResultUtils.Portability

[<Tests>]
let lsatTests =
    testList "LSAT tests" [
        testCase "service decode tests" <| fun _ ->
            let r = Service.ParseMany("a:0")
            Expect.isOk (Result.ToFSharpCoreResult r) "can parse single service"
            Expect.equal 1 (r |> Result.deref).Count ""
            Expect.equal "a" (r |> Result.deref).[0].Name ""
            let r = Service.ParseMany("a:0,b:1,c:0")
            Expect.isOk (Result.ToFSharpCoreResult r) "can parse multiple service"
            Expect.equal 3 (r |> Result.deref).Count ""
            Expect.equal "c" (r |> Result.deref).[2].Name ""
            Expect.equal 0uy (r |> Result.deref).[2].ServiceTier ""
            let r = Service.ParseMany ""
            Expect.isError (Result.ToFSharpCoreResult r) "can not parse empty service"
            let r = Service.ParseMany ":a"
            Expect.isError (Result.ToFSharpCoreResult r) "can not parse service missing name"
            let r = Service.ParseMany "a"
            Expect.isError (Result.ToFSharpCoreResult r) "can not parse service missing tier"
            let r = Service.ParseMany "a:"
            Expect.isError (Result.ToFSharpCoreResult r) "can not parse service with empty tier"
            let r = Service.ParseMany ",,"
            Expect.isError (Result.ToFSharpCoreResult r) "can not parse empty services"
            ()
            
        testList "check macaroon verification works in LSAT compliant way" [
            testCase "successful verification" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("service=my-service-name:0"))
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isTrue(v.Success) (sprintf "%A" v.Messages)
                ()
                
            testCase "successful verification with unknown service name" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("service=my-service-name:0,another-service-name:0"))
                caveats.Add(Caveat("service=my-service-name:0"))
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isTrue(v.Success) (sprintf "%A" v.Messages)
                
            testCase "successful verification with capabilities satisfier" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("service=my-service-name:0"))
                caveats.Add(Caveat("my-service-name_capabilities=read"))
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                satisfiers.Add(CapabilitiesSatisfier("my-service-name", "read") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isTrue(v.Success) (sprintf "%A" v.Messages)
                
            testCase "verification succeeds when caveats includes required capabilities" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("my-service-name_capabilities=read,write"))
                let satisfiers = ResizeArray()
                satisfiers.Add(CapabilitiesSatisfier("my-service-name", "read") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isTrue(v.Success) (sprintf "%A" v.Messages)
                
            testCase "failure case: different secret" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("service=unknown-service-name:0"))
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, "wrong secret key")
                Expect.isFalse(v.Success) ""
                
            testCase "failure case: different service name" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let caveats = ResizeArray()
                caveats.Add(Caveat("service=unknown-service-name:0"))
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isFalse(v.Success) ""
                
            testCase "failure case: verification fails if restriction gets loose then before" <| fun _ ->
                let secret = "My secret key"
                let identifier = "my macaroon identifier"
                let m = Macaroon("http://my.awesome.service", secret, identifier)
                let satisfiers = ResizeArray()
                satisfiers.Add(ServiceSatisfier("my-service-name") :> ISatisfier)
                
                let caveats = ResizeArray()
                // latter caveats has more power here. which is invalid for lsat.
                caveats.Add(Caveat("service=my-service-name:0"))
                caveats.Add(Caveat("service=my-service-name:0,another-service-name:0"))
                
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isFalse(v.Success) ""
                
                satisfiers.Add(CapabilitiesSatisfier("my-service-name", "read") :> ISatisfier)
                let caveats = ResizeArray()
                // latter caveats has more power here. which is invalid for lsat.
                caveats.Add(Caveat("my-service-name_capabilities = read"))
                caveats.Add(Caveat("my-service-name_capabilities = read,write"))
                let v = m.VerifyLSATCaveats(caveats, satisfiers, secret)
                Expect.isFalse(v.Success) ""
                
        ]
    ]
