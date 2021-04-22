namespace FSharpIL.Metadata

open System
open System.Reflection
open System.Runtime.CompilerServices

// TODO: Remove Null option and use voption instead.
// TODO: Use Delegate instead of Class.
type EventTypeTag =
    | Null = 0uy
    | Ref = 1uy
    | Spec = 2uy
    | ConcreteClass = 3uy
    | AbstractClass = 4uy
    | SealedClass = 5uy

type EventType = TaggedIndex<EventTypeTag>

/// <summary>(0x14) Represents a row in the <c>Event</c> table (II.22.12).</summary>
[<RequireQualifiedAccess>]
module EventType =
    let (|Null|TypeRef|TypeSpec|ConcreteClass|AbstractClass|SealedClass|) (eventType: EventType) =
        match eventType.Tag with
        | EventTypeTag.Ref -> TypeRef(eventType.ToRawIndex<TypeRef>())
        | EventTypeTag.Spec -> TypeSpec(eventType.ToRawIndex<TypeSpecRow>())
        | EventTypeTag.ConcreteClass -> ConcreteClass(eventType.ToRawIndex<ConcreteClassDef>())
        | EventTypeTag.AbstractClass -> AbstractClass(eventType.ToRawIndex<AbstractClassDef>())
        | EventTypeTag.SealedClass -> SealedClass(eventType.ToRawIndex<SealedClassDef>())
        | EventTypeTag.Null
        | _ -> Null

    let (|Def|Ref|Spec|) (eventType: EventType) =
        match eventType with
        | ConcreteClass(Index tdef)
        | AbstractClass(Index tdef)
        | SealedClass(Index tdef) -> Def(RawIndex<TypeDefRow> tdef)
        | TypeRef tref -> Ref tref
        | TypeSpec tspec -> Spec tspec
        | _ -> invalidArg "eventType" "Invalid event type"

    let Null = EventType(EventTypeTag.Null, 0)
    let Ref (eventType: RawIndex<TypeRef>) = eventType.ToTaggedIndex EventTypeTag.Ref
    let Spec (eventType: RawIndex<TypeSpecRow>) = eventType.ToTaggedIndex EventTypeTag.Spec
    let ConcreteClass (eventType: RawIndex<ConcreteClassDef>) = eventType.ToTaggedIndex EventTypeTag.ConcreteClass
    let AbstractClass (eventType: RawIndex<AbstractClassDef>) = eventType.ToTaggedIndex EventTypeTag.AbstractClass
    let SealedClass (eventType: RawIndex<SealedClassDef>) = eventType.ToTaggedIndex EventTypeTag.SealedClass

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type EventRow internal (flags: EventAttributes, name: Identifier, eventType: EventType) =
    /// <summary>Corresponds to the <c>EventFlags</c> column of the <c>Event</c> table (II.22.12).</summary>
    member _.Flags = flags
    member _.Name = name
    /// The type of the event.
    member _.EventType = eventType

    interface IEquatable<EventRow> with member _.Equals other = name = other.Name

// TODO: Make Event`1 a non-generic struct.
// TODO: Make Event`1 a normal struct to allow default value for Flags.
[<IsReadOnly; Struct>]
type Event<'Tag> =
    { Flags: SpecialName
      /// <summary>Corresponds to the <c>Name</c> column of the <c>Event</c> table (II.22.12).</summary>
      EventName: Identifier
      EventType: EventType }

    member internal this.Definition() = EventRow((|Flags|) this.Flags, this.EventName, this.EventType)

type InstanceEvent = Event<InstanceMethodTag>
type StaticEvent = Event<StaticMethodTag>

/// <summary>
/// Error used when there is a duplicate row in the <c>Event</c> table (11).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateEventError (event: EventRow) =
    inherit ValidationError()
    member _.Event = event
    override _.ToString() =
        sprintf
            "Unable to add event \"%O\", an event with the same name already exists"
            event.Name
