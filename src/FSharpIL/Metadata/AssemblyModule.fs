[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.Assembly

open System.Collections.Immutable

open FSharpIL

/// <summary>
/// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET
/// assembly.
/// </summary>
let setAssembly (builder: CliMetadataBuilder) assembly = builder.SetAssembly assembly

// TODO: Check to ensure that tfm refers to System.Runtime.Versioning.TargetFrameworkAttribute
/// <summary>Adds a <c>TargetFrameworkAttribute</c> to the current assembly specifying the target framework.</summary>
/// <param name="builder" />
/// <param name="assembly">The single row in the <c>Assembly</c> table of the CLI metadata.</param>
/// <param name="ctor">The constructor for a <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/>.</param>
/// <param name="tfm">The target framework moniker. For .NET 5, the value is <c>.NETCoreApp,Version=v5.0</c>.</param>
let setTargetFramework (builder: CliMetadataBuilder) (assembly: RawIndex<Assembly>) ctor tfm =
    let tfm' = FixedArg.Elem (SerString tfm)
    let value =
        { FixedArg = ImmutableArray.Create tfm'; NamedArg = ImmutableArray.Empty }
        |> builder.Blobs.CustomAttribute.TryAdd
        |> Result.any
        |> ValueSome
    // TODO: Check that the constructor is correct: has correct name, has this, has string parameter, etc.
    CustomAttribute.addTo builder (CustomAttributeParent.Assembly assembly) (CustomAttributeType.MethodRefDefault ctor) value
