namespace FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<Struct>]
type CustomAttributeCtor =
    val Constructor : MethodTok
    new (token) = { Constructor = token }

module CustomAttributeCtor =
    type Constructor = MethodReference<MethodKinds.ObjectConstructor>

    let Referenced (target: MethodTok<TypeReference<'Kind>, Constructor> when 'Kind :> TypeKinds.IHasConstructors) =
        CustomAttributeCtor target.Token

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      NamedArguments: ImmutableArray<NamedArg> }
