namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

type InstancePropertyMethodTag =
    // TODO: Consider adding | Null = 0uy to avoid having to use voption.
    | Instance = 1uy
    | Abstract = 2uy
    | Final = 3uy

type InstancePropertyMethod = TaggedIndex<InstancePropertyMethodTag>

[<RequireQualifiedAccess>]
module InstancePropertyMethod =
    let Instance (method: RawIndex<InstanceMethod>) = method.ToTaggedIndex InstancePropertyMethodTag.Instance

[<AutoOpen>]
module PropertyMethodPatterns =
    let inline private helper (method: TaggedIndex<_>) = method.ToRawIndex<MethodDefRow>()
    let (|InstancePropertyMethod|) (method: InstancePropertyMethod) = helper method
    let inline internal (|OptionalPropertyMethod|) (method: TaggedIndex<_> voption) =
        match method with
        | ValueSome method' -> helper method' |> ValueSome
        | ValueNone -> ValueNone

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module Property =
    let tryCreateInstanceRow
        builder
        (InstanceMemberOwner owner)
        (OptionalPropertyMethod getter: InstancePropertyMethod voption)
        (OptionalPropertyMethod setter: InstancePropertyMethod voption)
        (others: ImmutableArray<InstancePropertyMethod>)
        (property: InstanceProperty) =
        let others' = let mutable others = others in Unsafe.As<_, ImmutableArray<RawIndex<MethodDefRow>>> &others
        Unsafe.tryCreatePropertyRow
            builder
            owner
            (PropertyMethods(getter, setter, others'))
            property
    let inline createInstanceRow builder owner getter setter others property =
        tryCreateInstanceRow builder owner getter setter others property |> ValidationError.check
