namespace FSharpIL.Writing

open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures

[<Sealed>]
type CoreAssemblyMembers (octor: MethodCallTarget<_, _>, tfmctor: MethodCallTarget<_, _>) =
    member _.ObjectConstructor = octor
    member _.TargetFrameworkConstructor = tfmctor

[<Sealed>]
type CoreAssemblyReference (assembly: ReferencedAssembly) =
    static let netCoreLib = FileName.ofStr "System.Private.CoreLib"

    let referenceSystemType name =
        ReferencedType.ConcreteClass (
            TypeReferenceParent.Assembly assembly,
            ValueSome(Identifier.ofStr "System"),
            Identifier.ofStr name,
            GenericParamList.empty
        )

    let object = referenceSystemType "Object"
    let tfmattr =
        ReferencedType.SealedClass (
            TypeReferenceParent.Assembly assembly,
            ValueSome(Identifier.ofStr "System.Runtime.Versioning"),
            Identifier.ofStr "TargetFrameworkAttribute",
            GenericParamList.empty
        )

    let octor = ReferencedMethod.Constructor(ExternalVisibility.Public, ImmutableArray.Empty)
    let tfmctor =
        ReferencedMethod.Constructor (
            ExternalVisibility.Public,
            ImmutableArray.Create(MethodParameterType.Type PrimitiveType.String)
        )

    member _.Reference = assembly
    member _.Object = object
    member val ValueType = referenceSystemType "ValueType" :> ReferencedType
    member val Delegate = referenceSystemType "Delegate" :> ReferencedType
    member val Enum = referenceSystemType "Enum" :> ReferencedType
    member _.TargetFrameworkAttribute = tfmattr

    member this.AddReferencesTo(builder: CliModuleBuilder) =
        builder.ReferenceAssembly assembly

        validated {
            let! object' = builder.ReferenceType object
            let! _ = builder.ReferenceType this.ValueType
            let! _ = builder.ReferenceType this.Delegate
            let! _ = builder.ReferenceType this.Enum
            let! tfmattr' = builder.ReferenceType tfmattr

            let! octor' = object'.ReferenceMethod octor
            let! tfmctor' = tfmattr'.ReferenceMethod tfmctor
            return CoreAssemblyMembers(octor', tfmctor')
        }

    static member NetCore(version, publicKeyToken, ?hash) =
        CoreAssemblyReference
            { Version = version
              PublicKeyOrToken = PublicKeyToken publicKeyToken
              Name = netCoreLib
              Culture = ValueNone
              HashValue = defaultArg hash System.Collections.Immutable.ImmutableArray<byte>.Empty }
