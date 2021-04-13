namespace FSharpIL.Metadata

open System
open System.Reflection
open System.Runtime.CompilerServices

// TODO: Figure out if null event types should be allowed.
// TODO: Should Delegate be used instead of Class?
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

    let Null = EventType(EventTypeTag.Null, 0)
    let TypeRef (eventType: RawIndex<TypeSpecRow>) = eventType.ToTaggedIndex EventTypeTag.Ref
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
