module DotNetLightning.Tests.LSATTests

open System
open System.Linq
open Expecto
open DotNetLightning.Payment.LSAT
open Macaroons
open ResultUtils

// These tests go through the examples from the tutorial on 
// the original libmacaroons GitHub page at https://github.com/rescrv/libmacaroons
[<Tests>]
let macaroonTests =
    let Secret = "this is our super secret key; only we should know it";
    let Identifier = "we used our secret key";
    let Location = "http://mybank/";

    let Secret2 = "this is a different super-secret key; never use the same secret twice";
    let Identifier2 = "we used our other secret key";
    let Location2 = "http://mybank/"
    CryptoAlgorithm.initMacaroon()
    testList "macaroon sanity tests" [
        testCase "can create empty macaroon with signature" <| fun _ ->
            let m = Macaroon(Location, Secret, Identifier)
            Expect.equal Identifier (m.Identifier.ToString()) ""
            Expect.equal Location (m.Location.ToString()) ""
            Expect.equal("E3D9E02908526C4C0039AE15114115D97FDD68BF2BA379B342AAF0F617D0552F".ToLowerInvariant()) (m.Signature.ToString()) ""
            Expect.equal (0) (m.Caveats.Count) ""
        testCase "can Add third party caveat" <| fun _ ->
            try
                let m = new Macaroon(Location2, Secret2, Identifier2);
                m.AddFirstPartyCaveat("account = 3735928559") |> ignore

                // - just checking (this should although be covered in other tests) ...
                Expect.equal("1434e674ad84fdfdc9bc1aa00785325c8b6d57341fc7ce200ba4680c80786dda") (m.Signature.ToString()) ""

                // Act
                let caveat_key = "4; guaranteed random by a fair toss of the dice"
                // string predicate = "user = Alice";
                // # send_to_auth(caveat_key, predicate)
                // # identifier = recv_from_auth()
                let identifier = "this was how we remind auth of key/pred"

                m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier) |> ignore

                // Assert
                Expect.equal("d27db2fd1f22760e4c3dae8137e2d8fc1df6c0741c18aed4b97256bf78d1f55c") (m.Signature.ToString()) ""
         
                let expectedStringRepresentation =
                    [
                        @"Location = http://mybank/";
                        @"Identifier = we used our other secret key";
                        @"CId = account = 3735928559";
                        @"CId = this was how we remind auth of key/pred";
                        @"  VId = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA027FAuBYhtHwJ58FX6UlVNFtFsGxQHS7uD_w_dedwv4Jjw7UorCREw5rXbRqIKhr";
                        @"  Cl = http://auth.mybank/";
                        @"Signature = d27db2fd1f22760e4c3dae8137e2d8fc1df6c0741c18aed4b97256bf78d1f55c"
                        ""
                    ]
                    |> String.concat Environment.NewLine

                Expect.equal(expectedStringRepresentation) (m.Inspect()) ""

                let thirdPartyCaveats = m.ThirdPartyCaveats.ToList()
                Expect.equal 1 (thirdPartyCaveats.Count) ""
                Expect.equal "http://auth.mybank/" (thirdPartyCaveats.[0].Cl.ToString()) ""
                Expect.equal "this was how we remind auth of key/pred" (thirdPartyCaveats.[0].CId.ToString()) ""
            with
            | :? NotSupportedException ->
                // We do not (yet) support third party caveats in BouncyCastle build.
                // So this exception is fine.
                ()
            
        testCase "Can prepare for request" <| fun _ ->
            try
                // Arrange
                let m = new Macaroon(Location2, Secret2, Identifier2);
                m.AddFirstPartyCaveat("account = 3735928559") |> ignore

                let caveat_key = "4; guaranteed random by a fair toss of the dice";
                let identifier = "this was how we remind auth of key/pred";
                m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier) |> ignore

                let d = Macaroon("http://auth.mybank/", caveat_key, identifier)
                d.AddFirstPartyCaveat("time < 2015-01-01T00:00") |> ignore

                // Act
                let dp = m.PrepareForRequest(d)

                // Assert
                Expect.equal("82a80681f9f32d419af12f6a71787a1bac3ab199df934ed950ddf20c25ac8c65") (d.Signature.ToString()) ""
                Expect.equal("2eb01d0dd2b4475330739140188648cf25dda0425ea9f661f1574ca0a9eac54e") (dp.Signature.ToString()) ""
            with
            | :? NotSupportedException ->
                // We do not (yet) support third party caveats in BouncyCastle build.
                // So this exception is fine.
                ()
    ]

[<Tests>]
let lsatTests =
    testList "LSAT tests" [
        testCase "service decode tests" <| fun _ ->
            let r = Service.ParseMany("a:0")
            Expect.isOk r "can parse single service"
            Expect.equal 1 (r |> Result.deref).Count ""
            Expect.equal "a" (r |> Result.deref).[0].Name ""
            let r = Service.ParseMany("a:0,b:1,c:0")
            Expect.isOk r "can parse multiple service"
            Expect.equal 3 (r |> Result.deref).Count ""
            Expect.equal "c" (r |> Result.deref).[2].Name ""
            Expect.equal 0uy (r |> Result.deref).[2].ServiceTier ""
            let r = Service.ParseMany ""
            Expect.isError r "can not parse empty service"
            let r = Service.ParseMany ":a"
            Expect.isError r "can not parse service missing name"
            let r = Service.ParseMany "a"
            Expect.isError r "can not parse service missing tier"
            let r = Service.ParseMany "a:"
            Expect.isError r "can not parse service with empty tier"
            let r = Service.ParseMany ",,"
            Expect.isError r "can not parse empty services"
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
