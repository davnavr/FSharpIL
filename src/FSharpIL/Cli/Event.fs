namespace FSharpIL.Cli

open System

open FSharpIL.Metadata

open FSharpIL.Utilities.Compare

[<Sealed>]
type Event (name, etype, add, remove, raise, other) =
    member _.Flags = FSharpIL.Metadata.Tables.EventFlags.None
    member _.Name: Identifier = name
    member _.Type: TypeTok = etype
    member _.Add: DefinedMethod = add
    member _.Remove: DefinedMethod = remove
    member _.Raise: DefinedMethod voption = raise
    member _.Other: DefinedMethod list = other

    override _.GetHashCode() = HashCode.Combine(name, add, remove)

    interface IEquatable<Event> with
        member _.Equals other = name === other.Name

    override this.Equals obj =
        match obj with
        | :? Event as other -> this === other
        | _ -> false

module EventPatterns =
    let inline (|EventMethods|) (event: Event) = struct(event.Add, event.Remove, event.Raise, event.Other)
