namespace FSharpIL.Metadata

open System.Collections.Immutable

//module Method =

// TODO: Make tryAddRow functions use inref, since Definition() method is internal.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module InstanceMethod =
    let methodIndex (method: RawIndex<InstanceMethod>) = method.ChangeTag<MethodDefRow>()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module AbstractMethod =
    let methodIndex (method: RawIndex<AbstractMethod>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow builder (AbstractMethodParent owner) (method: AbstractMethod) =
        method.Definition() |> Unsafe.tryAddMethodDefRow<AbstractMethod> builder owner
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<RequireQualifiedAccess>]
module internal Constructor =
    let inline tryAddRow<'Tag, 'Flags, 'Signature>
        builder
        (StaticMemberParent owner)
        (method: Constructor<'Flags, 'Signature>)
        name
        signature
        =
        MethodDefRow (
            method.Body,
            method.ImplFlags.Value,
            method.Flags.Value,
            Identifier.ofStr name,
            signature,
            method.ParamList
        )
        |> Unsafe.tryAddMethodDefRow<'Tag> builder owner

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module ClassConstructor =
    let methodIndex (method: RawIndex<ClassConstructor>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow (builder: CliMetadataBuilder) owner (method: ClassConstructor) =
        let signature =
            MethodDefSignature (
                false,
                false,
                MethodCallingConventions.Default,
                ReturnTypeItem ReturnTypeVoid.Item,
                ImmutableArray.Empty
            )
            |> builder.Blobs.MethodDefSig.GetOrAdd
        Constructor.tryAddRow<ClassConstructor, _, _> builder owner method ".cctor" signature
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticMethod =
    let methodIndex (method: RawIndex<StaticMethod>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow builder (StaticMemberParent owner) (method: StaticMethod) =
        method.Definition() |> Unsafe.tryAddMethodDefRow<StaticMethod> builder owner
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check
