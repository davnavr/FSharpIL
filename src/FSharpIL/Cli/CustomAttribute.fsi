namespace FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<RequireQualifiedAccess>]
type CustomAttributeCtor =
    | Ref of ReferencedType * MethodReference<MethodKinds.ObjectConstructor>
    | Def of DefinedType * MethodDefinition<MethodKinds.ObjectConstructor>
    //| Spec of TypeSpecification * 

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

[<NoComparison; ReferenceEquality>]
type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      // TODO: How to ensure that the fields and properties actually exist?
      NamedArguments: ImmutableArray<NamedArg> }
