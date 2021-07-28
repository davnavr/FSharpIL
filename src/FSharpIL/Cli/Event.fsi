namespace FSharpIL.Cli

open FSharpIL.Metadata

/// Represents an event (I.8.11.4 and II.18).
[<Sealed>]
type Event =
    member Flags: FSharpIL.Metadata.Tables.EventFlags
    member Name: Identifier
    member Type: TypeTok
    /// <summary>The <c>add_</c> method used to add a handler for an event.</summary>
    member Add: DefinedMethod
    /// <summary>The <c>remove_</c> method used to remove a handler for an event.</summary>
    member Remove: DefinedMethod
    /// <summary>The optional <c>raise_</c> method used to indicate that an event has occured.</summary>
    member Raise: DefinedMethod voption
    member Other: DefinedMethod list

    internal new:
        name: Identifier *
        etype: TypeTok *
        add: DefinedMethod *
        remove: DefinedMethod *
        raise: DefinedMethod voption *
        other: DefinedMethod list -> Event

    interface System.IEquatable<Event>

    override GetHashCode: unit -> int32
    override Equals: obj -> bool

[<AutoOpen>]
module EventPatterns =
    val inline (|EventMethods|) :
        event: Event -> struct(DefinedMethod * DefinedMethod * DefinedMethod voption * DefinedMethod list)
