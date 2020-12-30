namespace ResultUtils

open System

[<AutoOpen>]
module OptionCE =
  type OptionBuilder() =
    member __.Return<'T>(value: 'T): Option<'T> =
        Some value

    member __.ReturnFrom<'T>(opt: Option<'T>): Option<'T> =
        opt

    member this.Zero(): Option<unit> =
        Some ()

    member __.Bind<'T, 'U>(opt: Option<'T>, binder: 'T -> Option<'U>)
                              : Option<'U> =
        Option.bind binder opt

    member __.Delay<'T>(continuation: unit -> Option<'T>)
                           : unit -> Option<'T> =
        continuation

    member __.Run<'T>(continuation: unit -> Option<'T>)
                         : Option<'T> =
        continuation()

    member this.Combine<'T>(opt: Option<unit>, binder: unit -> Option<'T>)
                               : Option<'T> =
        this.Bind(opt, binder)

    member this.TryWith<'T>(generator: unit -> Option<'T>, handler: exn -> Option<'T>)
                               : Option<'T> =
        try
            this.Run generator
        with
        | e -> handler e

    member this.TryFinally<'T>(generator: unit -> Option<'T>, final: unit -> unit)
                                  : Option<'T> =
        try
            this.Run generator
        finally
            final()

    member this.Using<'T, 'U when 'T :> IDisposable>(resource: 'T, binder: 'T -> Option<'U>)
                                                        : Option<'U> =
        this.TryFinally (
            (fun () -> binder resource),
            (fun () ->
                if not <| obj.ReferenceEquals(resource, null) then
                    resource.Dispose ()
            )
        )

    member this.While(guard: unit -> bool, generator: unit -> Option<unit>)
                         : Option<unit> =
      if not <| guard () then
          this.Zero ()
      else
          this.Bind(this.Run generator, fun () -> this.While (guard, generator))

    member this.For<'T, 'Sequence when 'Sequence :> seq<'T> >(sequence: 'Sequence, binder: 'T -> Option<unit>)
                                                             : Option<unit> =
        this.Using(sequence.GetEnumerator (), fun enumerator ->
            this.While(enumerator.MoveNext, this.Delay(fun () -> binder enumerator.Current))
        )

[<AutoOpen>]
module OptionCEExtensions =
    let option = OptionBuilder()

