namespace FSharpIL.Metadata

open System.Collections.Immutable

//module Method =

// TODO: Make tryAddRow functions use inref, since Definition() method is internal.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module InstanceMethod =
    let methodIndex (method: RawIndex<InstanceMethod>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow builder (InstanceMemberOwner owner) (method: InstanceMethod) =
        method.Definition() |> Unsafe.tryAddMethodDefRow<InstanceMethod> builder owner
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module AbstractMethod =
    let methodIndex (method: RawIndex<AbstractMethod>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow builder (AbstractMethodOwner owner) (method: AbstractMethod) =
        method.Definition() |> Unsafe.tryAddMethodDefRow<AbstractMethod> builder owner
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<RequireQualifiedAccess>]
module internal Constructor =
    let inline tryAddRow<'Tag, 'Flags, 'Signature>
        (builder: CliMetadataBuilder)
        owner
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
module ObjectConstructor =
    let methodIndex (method: RawIndex<ObjectConstructor>) = method.ChangeTag<MethodDefRow>()
    // TODO: Might need to use inref when adding ObjectConstructor since it uses an internal method.
    let tryAddRow (builder: CliMetadataBuilder) (InstanceMemberOwner owner) (method: ObjectConstructor) =
        method.Signature.ChangeTag() |> Constructor.tryAddRow<ObjectConstructor, _, _> builder owner method ".cctor"
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module ClassConstructor =
    let methodIndex (method: RawIndex<ClassConstructor>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow (builder: CliMetadataBuilder) (StaticMemberOwner owner) (method: ClassConstructor) =
        MethodDefSignature (
            false,
            false,
            MethodCallingConventions.Default,
            ReturnTypeItem ReturnTypeVoid.Item,
            ImmutableArray.Empty
        )
        |> builder.Blobs.MethodDefSig.GetOrAdd
        |> Constructor.tryAddRow<ClassConstructor, _, _> builder owner method ".cctor"
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticMethod =
    let methodIndex (method: RawIndex<StaticMethod>) = method.ChangeTag<MethodDefRow>()
    let inline tryAddRow builder (StaticMemberOwner owner) (method: StaticMethod) =
        method.Definition() |> Unsafe.tryAddMethodDefRow<StaticMethod> builder owner
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check
