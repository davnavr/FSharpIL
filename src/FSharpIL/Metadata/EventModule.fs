[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.Event

open System.Collections.Immutable
open System.Runtime.CompilerServices

let tryCreateInstanceRow
    builder
    (InstanceMemberOwner owner)
    (InstanceMethodIndex add: InstanceMethodIndex)
    (InstanceMethodIndex remove: InstanceMethodIndex)
    (OptionalMethodIndex fire: InstanceMethodIndex voption)
    (others: ImmutableArray<InstanceMethodIndex>)
    (event: InstanceEvent) =
    let mutable others' = Array.init others.Length (fun i -> others.[i].ToRawIndex<MethodDefRow>())
    let methods = EventMethods(add, remove, fire, Unsafe.As &others')
    Unsafe.tryCreateEventRow
        builder
        owner
        &methods
        &event

let inline createInstanceRow builer owner add remove fire others event =
    tryCreateInstanceRow builer owner add remove fire others event |> ValidationError.check

let inline tryCreateInstance
    builder
    (InstanceMemberOwner owner)
    (event: InstanceEvent)
    visibility
    vTableLayout
    hideBySig
    isVirtual
    addBody
    removeBody
    =
    let mflags = InstanceMethodFlags(visibility, SpecialName, vTableLayout, hideBySig, isVirtual).Value
    Unsafe.tryCreateEvent builder owner &event mflags true addBody removeBody

let inline createInstance builder owner event visibility vTableLayout hideBySig isVirtual addBody removeBody =
    tryCreateInstance builder owner event visibility vTableLayout hideBySig isVirtual addBody removeBody |> ValidationError.check
