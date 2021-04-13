[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.Property

open System.Collections.Immutable
open System.Runtime.CompilerServices

let tryCreateInstanceRow
    builder
    (InstanceMemberOwner owner)
    (OptionalMethodIndex getter: InstanceMethodIndex voption)
    (OptionalMethodIndex setter: InstanceMethodIndex voption)
    (others: ImmutableArray<InstanceMethodIndex>)
    (property: InstanceProperty) =
    let mutable others' = Array.init others.Length (fun i -> others.[i].ToRawIndex<MethodDefRow>())
    Unsafe.tryCreatePropertyRow
        builder
        owner
        (PropertyMethods(getter, setter, Unsafe.As &others'))
        property

let inline createInstanceRow builder owner getter setter others property =
    tryCreateInstanceRow builder owner getter setter others property |> ValidationError.check

//let tryCreateStaticRow
