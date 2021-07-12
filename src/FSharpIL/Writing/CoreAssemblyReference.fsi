namespace FSharpIL.Writing

open FSharpIL.Cli
open FSharpIL.Metadata

[<Sealed>]
type CoreAssemblyMembers =
    /// <summary>
    /// The constructor for <see cref="T:System.Object"/>, called in the constructor of all directly derived types.
    /// </summary>
    member ObjectConstructor: MethodCallTarget
    
    /// <summary>
    /// The constructor of the <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/> type, which accepts a string
    /// containing the name of the target framework and its version.
    /// </summary>
    member TargetFrameworkConstructor: MethodCallTarget

/// <summary>Represents the assembly containing core types such as <see cref="T:System.Object"/>.</summary>
[<Sealed>]
type CoreAssemblyReference =
    member Reference: ReferencedAssembly
    /// <summary>The core type <see cref="T:System.Object"/>, which serves as the base type for all types.</summary>
    member Object: ReferencedType
    /// <summary>
    /// The core type <see cref="T:System.ValueType"/>, which serves as the base type for all value types (II.13).
    /// </summary>
    member ValueType: ReferencedType
    /// <summary>
    /// The core type <see cref="T:System.Delegate"/>, which serves as the base type for all delegates (II.14.6).
    /// </summary>
    member Delegate: ReferencedType
    /// <summary>
    /// The core type <see cref="T:System.Enum"/>, which serves as the base type for all enumeration types (II.14.3).
    /// </summary>
    member Enum: ReferencedType
    /// <summary>
    /// The <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/> type, which identifies the target framework that
    /// the current assembly or module was compiled with (.NET Core, .NET Framework, .NET standard, etc.).
    /// </summary>
    member TargetFrameworkAttribute: ReferencedType

    new: assembly: ReferencedAssembly -> CoreAssemblyReference

    /// <summary>
    /// Adds a reference to <c>System.Private.CoreLib</c>, which is the core assembly for .NET Core and .NET 5+.
    /// </summary>
    static member NetCore:
        version: AssemblyVersion *
        publicKeyToken: PublicKeyToken *
        ?hash: System.Collections.Immutable.ImmutableArray<byte> -> CoreAssemblyReference
