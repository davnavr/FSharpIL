namespace FSharpIL.Cli

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<RequireQualifiedAccess>]
type CustomAttributeCtor =
    | Ref of MethodCallTarget<ReferencedType, MethodReference<MethodKinds.ObjectConstructor>>
    | Def of MethodCallTarget<DefinedType, MethodDefinition<MethodKinds.ObjectConstructor>>
    //| Spec of TypeSpecification * 

    static member Referenced(target: MethodCallTarget<TypeReference<'Kind>, _> when 'Kind :> TypeKinds.IHasConstructor) =
        CustomAttributeCtor.Ref(MethodCallTarget<_, _>(target.Owner :> ReferencedType, target.Method))

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      NamedArguments: ImmutableArray<NamedArg> }
