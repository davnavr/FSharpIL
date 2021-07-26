namespace FSharpIL.Writing
// TODO: Make this file contain a module named CoreLib

open FSharpIL.Cli
open FSharpIL.Metadata

[<Sealed>]
type CoreAssemblyMembers =
    member Array: ReferencedTypeMembers<TypeKinds.AbstractClass>

    // TODO: Make this a field of some ObjectMembers class instead.
    /// <summary>
    /// The constructor for <see cref="T:System.Object"/>, called in the constructor of all directly derived types.
    /// </summary>
    member ObjectConstructor: MethodTok<TypeReference<TypeKinds.ConcreteClass>, MethodReference<MethodKinds.ObjectConstructor>>

    /// <summary>
    /// The constructor of the <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/> type, which accepts a string
    /// containing the name of the target framework and its version.
    /// </summary>
    member TargetFrameworkConstructor:
        MethodTok<TypeReference<TypeKinds.SealedClass>, MethodReference<MethodKinds.ObjectConstructor>>

/// <summary>Represents the assembly containing core types such as <see cref="T:System.Object"/>.</summary>
[<Sealed>]
type CoreAssemblyReference =
    member Reference: ReferencedAssembly
    /// <summary>The core type <see cref="T:System.Object"/>, which serves as the base type for all types.</summary>
    member Object: TypeReference<TypeKinds.ConcreteClass>
    /// <summary>
    /// The core type <see cref="T:System.ValueType"/>, which serves as the base type for all value types (II.13).
    /// </summary>
    member ValueType: TypeReference
    /// <summary>
    /// The core type <see cref="T:System.Delegate"/>, which serves as the base type for all delegates (II.14.6).
    /// </summary>
    member Delegate: TypeReference
    /// <summary>
    /// The core type <see cref="T:System.Enum"/>, which serves as the base type for all enumeration types (II.14.3).
    /// </summary>
    member Enum: TypeReference
    /// <summary>
    /// The <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/> type, which identifies the target framework that
    /// the current assembly or module was compiled with (.NET Core, .NET Framework, .NET standard, etc.).
    /// </summary>
    member TargetFrameworkAttribute: TypeReference<TypeKinds.SealedClass>
    /// <summary>
    /// The core type <see cref="T:System.Array"/>, which provides many static methods that compilers can use to create arrays.
    /// </summary>
    member Array: TypeReference<TypeKinds.AbstractClass>

    new: assembly: ReferencedAssembly -> CoreAssemblyReference

    member Assembly : ReferencedAssembly

    /// <summary>
    /// Adds a reference to <c>System.Private.CoreLib</c>, which is the core assembly for .NET Core and .NET 5+.
    /// </summary>
    static member NetCore:
        version: FSharpIL.Metadata.Tables.AssemblyVersion *
        publicKeyToken: PublicKeyToken *
        ?hash: System.Collections.Immutable.ImmutableArray<byte> -> CoreAssemblyReference

    member AddReferencesTo: CliModuleBuilder -> ValidationResult<CoreAssemblyMembers>
