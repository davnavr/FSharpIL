namespace FSharpIL.Writing

open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata

[<Sealed>]
type CoreAssemblyMembers
    (
        array: ReferencedTypeMembers<_>,
        del: ReferencedTypeMembers<_>,
        octor: MethodTok<_, _>,
        tfmctor: MethodTok<_, _>
    )
    =
    member _.Array = array
    member _.Delegate = del
    member _.ObjectConstructor = octor
    member _.TargetFrameworkConstructor = tfmctor

[<Sealed>]
type CoreAssemblyReference (assembly: ReferencedAssembly) =
    static let netCoreLib = FileName.ofStr "System.Private.CoreLib"
    static let system = ValueSome(Identifier.ofStr "System")

    let object =
        TypeReference.ConcreteClass (
            TypeReferenceParent.Assembly assembly,
            system,
            Identifier.ofStr "Object"
        )

    let tfmattr =
        TypeReference.SealedClass (
            TypeReferenceParent.Assembly assembly,
            ValueSome(Identifier.ofStr "System.Runtime.Versioning"),
            Identifier.ofStr "TargetFrameworkAttribute"
        )

    let array =
        TypeReference.AbstractClass (
            TypeReferenceParent.Assembly assembly,
            system,
            Identifier.ofStr "Array"
        )

    let del =
        TypeReference.AbstractClass (
            TypeReferenceParent.Assembly assembly,
            system,
            Identifier.ofStr "Delegate"
        )

    let octor = ReferencedMethod.Constructor(ExternalVisibility.Public, ImmutableArray.Empty)
    let tfmctor =
        ReferencedMethod.Constructor (
            ExternalVisibility.Public,
            ImmutableArray.Create(ParameterType.T PrimitiveType.String)
        )

    let referenceSystemType name =
        { TypeReference.ResolutionScope = TypeReferenceParent.Assembly assembly
          Flags = ValueNone
          TypeNamespace = system
          TypeName = Identifier.ofStr name }

    member _.Reference = assembly
    member _.Object = object
    member val ValueType = referenceSystemType "ValueType"
    member _.Delegate = del
    member val Enum = referenceSystemType "Enum"
    member _.TargetFrameworkAttribute = tfmattr
    member _.Array = array

    member _.Assembly = assembly

    member this.AddReferencesTo(builder: CliModuleBuilder) =
        builder.ReferenceAssembly assembly

        validated {
            let! object' = builder.ReferenceType object
            let! _ = builder.ReferenceType(ReferencedType.Reference this.ValueType)
            let! del' = builder.ReferenceType this.Delegate
            let! _ = builder.ReferenceType(ReferencedType.Reference this.Enum)
            let! tfmattr' = builder.ReferenceType tfmattr
            let! array' = builder.ReferenceType array

            let! octor' = object'.ReferenceMethod octor
            let! tfmctor' = tfmattr'.ReferenceMethod tfmctor

            return CoreAssemblyMembers(array', del', octor', tfmctor')
        }

    static member NetCore(version, publicKeyToken, ?hash) =
        CoreAssemblyReference
            { Version = version
              PublicKeyOrToken = PublicKeyToken publicKeyToken
              Name = netCoreLib
              Culture = ValueNone
              HashValue = defaultArg hash System.Collections.Immutable.ImmutableArray<byte>.Empty }
