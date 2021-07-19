namespace FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<RequireQualifiedAccess>]
type CustomAttributeCtor =
    | Ref of MethodCallTarget<TypeReference, MethodReference<MethodKinds.ObjectConstructor>>
    | Def of MethodCallTarget<TypeDefinition, MethodDefinition<MethodKinds.ObjectConstructor>>
    //| Spec of TypeSpecification * 

    static member Referenced :
        target: MethodCallTarget<TypeReference<'Kind>, MethodReference<MethodKinds.ObjectConstructor>> ->
            CustomAttributeCtor when 'Kind :> TypeKinds.IHasConstructor

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

[<NoComparison; ReferenceEquality>]
type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      // TODO: How to ensure that the fields and properties actually exist?
      NamedArguments: ImmutableArray<NamedArg> }
