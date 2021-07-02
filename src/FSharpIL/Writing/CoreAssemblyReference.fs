namespace FSharpIL.Writing

open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures

[<Sealed>]
type CoreAssemblyMembers (octor: MethodCallTarget, tfmctor: MethodCallTarget) =
    member _.ObjectConstructor = octor
    member _.TargetFrameworkConstructor = tfmctor

[<Sealed>]
type CoreAssemblyReference (assembly: AssemblyReference) =
    static let netCoreLib = FileName.ofStr "System.Private.CoreLib"

    let referenceCoreType ns =
        let ns' = ValueSome(Identifier.ofStr ns)
        fun name ->
            ReferencedType.ConcreteClass (
                TypeReferenceParent.Assembly assembly,
                ns',
                Identifier.ofStr name
            )
            :> ReferencedType

    let referenceSystemType name = referenceCoreType "System" name

    let object = referenceSystemType "Object"
    let tfmattr = referenceCoreType "System.Runtime.Versioning" "TargetFrameworkAttribute"

    let octor = ReferencedMethod.Constructor(ExternalVisibility.Public, ImmutableArray.Empty)
    let tfmctor =
        let str = ParamItem.Param(ImmutableArray.Empty, EncodedType.String)
        ReferencedMethod.Constructor(ExternalVisibility.Public, ImmutableArray.Create str)

    member _.Reference = assembly
    member _.Object = object
    member val ValueType = referenceSystemType "ValueType"
    member val Delegate = referenceSystemType "Delegate"
    member val Enum = referenceSystemType "Enum"
    member _.TargetFrameworkAttribute = tfmattr

    member this.AddReferencesTo(builder: ModuleBuilder) =
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
