[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.CliMetadata

open System
open System.Collections.Immutable

/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<AbstractClass; Sealed>]
type Unsafe = class
    static member AddTypeDef<'Tag>
        (
            builder: CliMetadataBuilder,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        ): Result<TypeDefIndex<'Tag>, _> =
        let row =
            TypeDefRow (
                flags,
                typeName,
                typeNamespace,
                extends,
                parent
            )
        match builder.TypeDef.TryAdd row with
        | ValueSome index -> TypeDefIndex<'Tag> index |> Ok
        | ValueNone -> DuplicateTypeDefError row :> ValidationError |> Error

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends) =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, None)

    // ChangeTag
end

// TODO: Figure out where to put all of these miscellaneous helper functions.

// TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
let addCustomAttribute (builder: CliMetadataBuilder) attr = builder.CustomAttribute.Add attr

/// <summary>
/// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET
/// assembly.
/// </summary>
let setAssembly (builder: CliMetadataBuilder) assembly = builder.SetAssembly assembly

/// <summary>Sets the entrypoint of the assembly.</summary>
/// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
let setEntryPointToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
/// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
let setEntryPoint builder main = setEntryPointToken builder (EntryPointToken.ValidEntryPoint main)

// TODO: Check to ensure that tfm referes to System.Runtime.Versioning.TargetFrameworkAttribute
/// <summary>Adds a <c>TargetFrameworkAttribute</c> to the current assembly specifying the target framework.</summary>
/// <param name="builder" />
/// <param name="assembly">Dummy object used to guarantee that the CLI metadata is an assembly.</param>
/// <param name="ctor">The constructor for a <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/>.</param>
/// <param name="tfm">The target framework moniker. For .NET 5, the value is <c>.NETCoreApp,Version=v5.0</c>.</param>
let setTargetFramework builder (assembly: AssemblyIndex) (ctor: MemberRefIndex<_>) tfm =
    let tfm' = FixedArg.Elem (SerString tfm)
    // TODO: Check that the constructor is correct.
    { Parent = CustomAttributeParent.Assembly assembly
      Type = CustomAttributeType.MethodRefDefault ctor
      Value = Some { FixedArg = ImmutableArray.Create tfm'; NamedArg = ImmutableArray.Empty } }
    |> addCustomAttribute builder

/// <summary>
/// Applies the given function to each string in the <c>#Strings</c> heap referenced in
/// the CLI metadata tables (II.24.2.6).
/// </summary>
let inline internal iterStrings action (metadata: CliMetadata) =
    string metadata.Module.Name |> action

    for tref in metadata.TypeRef.Rows do
        string tref.TypeName |> action
        action tref.TypeNamespace

    for tdef in metadata.TypeDef.Rows do
        string tdef.TypeName |> action
        action tdef.TypeNamespace

    for field in metadata.Field.Rows do
        string field.Name |> action

    for method in metadata.MethodDef.Rows do
        string method.Name |> action

    for _, param in metadata.Param do
        action param.ParamName



    for mref in metadata.MemberRef.Rows do
        match mref with
        | MethodRefDefault { MemberName = name }
        | MethodRefGeneric { MemberName = name }
        | MethodRefVarArg { MemberName = name } ->
            string name |> action

    // ModuleRef table not necessary, since its names will correspond to names used in the File table

    match metadata.Assembly with
    | ValueSome assembly ->
        string assembly.Name |> action
        string assembly.Culture |> action
    | ValueNone -> ()

    for assembly in metadata.AssemblyRef.Rows do
        string assembly.Name |> action
        string assembly.Culture |> action

    for { FileName = name } in metadata.File.Rows do
        string name |> action
