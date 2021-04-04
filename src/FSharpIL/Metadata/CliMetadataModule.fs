[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.CliMetadata

open System
open System.Collections.Immutable

open FSharpIL

/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<AbstractClass; Sealed>]
[<Obsolete>]
type Unsafe = class
    static member AddTypeDef<'Tag>
        (
            builder: CliMetadataBuilder,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        ): Result<RawIndex<'Tag>, _> =
        let row =
            TypeDefRow (
                flags,
                typeName,
                typeNamespace,
                extends,
                parent
            )
        match builder.TypeDef.TryAdd row with
        | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
        | ValueNone -> DuplicateTypeDefError row :> ValidationError |> Error

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends) =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, ValueNone)

    // ChangeTag
end

[<Obsolete>]
let addCustomAttribute (builder: CliMetadataBuilder) (attr: CustomAttribute) = builder.CustomAttribute.Add &attr

/// <summary>
/// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET
/// assembly.
/// </summary>
[<Obsolete>]
let setAssembly (builder: CliMetadataBuilder) assembly = builder.SetAssembly assembly

[<Obsolete>]
/// <summary>Sets the entrypoint of the assembly.</summary>
let setEntryPointToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

[<Obsolete>]
/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
let setEntryPoint builder main = setEntryPointToken builder (EntryPointToken.ValidEntryPoint main)

// TODO: Check to ensure that tfm refers to System.Runtime.Versioning.TargetFrameworkAttribute
/// <summary>Adds a <c>TargetFrameworkAttribute</c> to the current assembly specifying the target framework.</summary>
/// <param name="builder" />
/// <param name="assembly">The single row in the <c>Assembly</c> table of the CLI metadata.</param>
/// <param name="ctor">The constructor for a <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/>.</param>
/// <param name="tfm">The target framework moniker. For .NET 5, the value is <c>.NETCoreApp,Version=v5.0</c>.</param>
[<Obsolete>]
let setTargetFramework (builder: CliMetadataBuilder) (assembly: RawIndex<Assembly>) (ctor: RawIndex<_>) tfm =
    let tfm' = FixedArg.Elem (SerString tfm)
    // TODO: Check that the constructor is correct.
    { Parent = CustomAttributeParent.Assembly assembly
      Type = CustomAttributeType.MethodRefDefault ctor
      Value =
        { FixedArg = ImmutableArray.Create tfm'; NamedArg = ImmutableArray.Empty }
        |> builder.Blobs.CustomAttribute.TryAdd
        |> Result.any
        |> ValueSome }
    |> addCustomAttribute builder

/// <summary>
/// Applies the given function to each string in the <c>#Strings</c> heap referenced in
/// the CLI metadata tables (II.24.2.6).
/// </summary>
let inline internal iterStrings action (metadata: CliMetadata) =
    metadata.Module.Name.ToString() |> action

    for tref in metadata.TypeRef.Rows do
        tref.TypeName.ToString() |> action
        action tref.TypeNamespace

    for tdef in metadata.TypeDef.Rows do
        tdef.TypeName.ToString() |> action
        action tdef.TypeNamespace

    for field in metadata.Field.Rows do
        field.Name.ToString() |> action

    for method in metadata.MethodDef.Rows do
        method.Name.ToString() |> action

    for _, param in metadata.Param do
        action param.ParamName



    for mref in metadata.MemberRef.Rows do
        mref.Name.ToString() |> action

    for property in metadata.Property.Rows do
        property.Name.ToString() |> action

    match metadata.Assembly with
    | ValueSome assembly ->
        assembly.Name.ToString() |> action
        assembly.Culture.ToString() |> action
    | ValueNone -> ()

    for assembly in metadata.AssemblyRef.Rows do
        assembly.Name.ToString() |> action
        assembly.Culture.ToString() |> action

    for { FileName = name } in metadata.File.Rows do
        name.ToString() |> action

    for gparam in metadata.GenericParam.Rows do
        gparam.Name.ToString() |> action
