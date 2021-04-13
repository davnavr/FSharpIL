namespace FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

type MethodSemanticsFlags =
    | Setter = 1us
    | Getter = 2us
    | Other = 4us
    | AddOn = 8us
    | RemoveOn = 0x10us
    | Fire = 0x20us

type MethodAssociationTag =
    | Property = 1uy
    | Event = 2uy

/// <summary>Indicates whether a method is associated with an <c>Event</c> or a <c>Property</c> (II.22.28).</summary>
type MethodAssociation = TaggedIndex<MethodAssociationTag>

[<RequireQualifiedAccess>]
module MethodAssociation =
    let (|Event|Property|) (association: MethodAssociation) =
        match association.Tag with
        | MethodAssociationTag.Event -> Event(association.ToRawIndex<EventRow>())
        | MethodAssociationTag.Property -> Property(association.ToRawIndex<PropertyRow>())
        | bad -> sprintf "Invalid method association kind %A" bad |> invalidArg "association"
    let Event (index: RawIndex<EventRow>) = index.ToTaggedIndex MethodAssociationTag.Event
    let Property (index: RawIndex<PropertyRow>) = index.ToTaggedIndex MethodAssociationTag.Property

/// <summary>(0x18) Represents a row in the <c>MethodSemantics</c> table (II.22.28).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodSemanticsRow internal
    (
        semantics: MethodSemanticsFlags,
        method: RawIndex<MethodDefRow>,
        assoc: MethodAssociation
    ) =
    member _.Semantics = semantics
    member _.Method = method
    member _.Association = assoc

// TODO: How to enforce that the Method column is for a property or event defined on the same class?
// TODO: Do we need to enforce that getters and setters are static for "static" properties? Maybe provide helper functions for this?
// TODO: Do we need to ensure names of "other" methods do not conflict with get_ and set_, or are the flags in MethodSemantics enough?
[<IsReadOnly; Struct>]
type PropertyMethods
    (
        getter: RawIndex<MethodDefRow> voption,
        setter: RawIndex<MethodDefRow> voption,
        others: ImmutableArray<RawIndex<MethodDefRow>>
    ) =
    new (getter, setter) = PropertyMethods(getter, setter, ImmutableArray.Empty)
    member _.Getter = getter
    member _.Setter = setter
    member _.Others = others

// TODO: Figure out where raise_ methods go.
// TODO: Do we need to ensure names of "other" methods do not conflict with add_, remove_, and fire_, or are the flags in MethodSemantics enough?
[<IsReadOnly; Struct>]
type EventMethods
    (
        addOn: RawIndex<MethodDefRow>,
        removeOn: RawIndex<MethodDefRow>,
        fire: RawIndex<MethodDefRow> voption,
        others: ImmutableArray<RawIndex<MethodDefRow>>
    ) =
    /// <summary>Corresponds to the <c>add_</c> method used to subscribe to the event.</summary>
    member _.AddOn = addOn
    /// <summary>Corresponds to the <c>remove_</c> method used to unsubscribe from the event.</summary>
    member _.RemoveOn = removeOn
    /// <summary>Corresponds to the optional <c>fire_</c> method.</summary>
    member _.Fire = fire
    member _.Others = others

// TODO: Figure out if indices into the MethodSemantics table are used.
[<Sealed>]
type MethodSemanticsTableBuilder internal () =
    let semantics = ImmutableArray.CreateBuilder<MethodSemanticsRow>()
    let events = Dictionary<RawIndex<EventRow>, EventMethods>()
    let properties = Dictionary<RawIndex<PropertyRow>, PropertyMethods>()

    member _.Count = semantics.Count

    member private _.AddSemantics(flags, method, assoc) = semantics.Add(MethodSemanticsRow(flags, method, assoc))

    member inline private this.AddPropertyMethod(flags, method, property) =
        this.AddSemantics(flags, method, MethodAssociation.Property property)

    member inline private this.AddEventMethod(flags, method, event) =
        this.AddSemantics(flags, method, MethodAssociation.Event event)

    member internal this.TryAddProperty(property, methods: inref<_>) =
        let success = properties.TryAdd(property, methods)
        if success then
            match methods.Getter with
            | ValueSome getter -> this.AddPropertyMethod(MethodSemanticsFlags.Getter, getter, property)
            | _ -> ()

            match methods.Setter with
            | ValueSome setter -> this.AddPropertyMethod(MethodSemanticsFlags.Setter, setter, property)
            | _ -> ()

            for other in methods.Others do this.AddPropertyMethod(MethodSemanticsFlags.Other, other, property)
        success

    member internal this.TryAddEvent(event, methods: inref<_>) =
        let success = events.TryAdd(event, methods)
        if success then
            this.AddEventMethod(MethodSemanticsFlags.AddOn, methods.AddOn, event)
            this.AddEventMethod(MethodSemanticsFlags.RemoveOn, methods.RemoveOn, event)

            match methods.Fire with
            | ValueSome fire -> this.AddEventMethod(MethodSemanticsFlags.Fire, fire, event)
            | _ -> ()

            for other in methods.Others do this.AddEventMethod(MethodSemanticsFlags.Other, other, event)
        success

    member _.ToImmutable() = MetadataTable(semantics.ToImmutable())

/// <category>Errors</category>
[<Sealed>]
type ExistingMethodSemanticsError (association: MethodAssociation) =
    inherit ValidationError()
    new (property) = ExistingMethodSemanticsError(MethodAssociation.Property property)
    new (event) = ExistingMethodSemanticsError(MethodAssociation.Event event)
    member _.Association = association
