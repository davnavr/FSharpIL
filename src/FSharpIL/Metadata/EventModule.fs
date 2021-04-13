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
    Unsafe.tryCreateEventRow
        builder
        owner
        (EventMethods(add, remove, fire, Unsafe.As &others'))
        event

let inline createInstanceRow builer owner add remove fire others event =
    tryCreateInstanceRow builer owner add remove fire others event |> ValidationError.check

//let tryCreateAddMethod
