namespace FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<System.Runtime.CompilerServices.IsReadOnly; Struct; NoComparison; StructuralEquality>]
type CustomAttributeCtor =
    val Constructor: MethodTok

    interface System.IEquatable<CustomAttributeCtor>

[<RequireQualifiedAccess>]
module CustomAttributeCtor =
    type Constructor = MethodReference<MethodKinds.ObjectConstructor>

    val Referenced : target: MethodTok<'Kind, Constructor> -> CustomAttributeCtor when 'Kind :> TypeKinds.IHasConstructor

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

[<NoComparison; ReferenceEquality>]
type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      // TODO: How to ensure that the fields and properties actually exist?
      NamedArguments: ImmutableArray<NamedArg> }
