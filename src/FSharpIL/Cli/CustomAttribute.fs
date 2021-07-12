namespace FSharpIL.Cli

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

[<RequireQualifiedAccess>]
type CustomAttributeCtor =
    | Ref of ReferencedType * MethodReference<MethodKinds.ObjectConstructor>
    | Def of DefinedType * MethodDefinition<MethodKinds.ObjectConstructor>
    //| Spec of TypeSpecification * 

type FixedArgSource = int32 -> Identifier voption -> ElemType -> Result<FixedArg, IValidationError voption>

type CustomAttribute =
    { Constructor: CustomAttributeCtor
      FixedArguments: FixedArgSource
      NamedArguments: ImmutableArray<NamedArg> }

[<System.Obsolete>]
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type CustomAttributeParent (parent: obj) =
    member _.Parent = parent

    member _.Equals(other: CustomAttributeParent) = parent.Equals other.Parent

    override _.GetHashCode() = parent.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? CustomAttributeParent as other -> this.Equals other
        | _ -> false

    interface System.IEquatable<CustomAttributeParent> with member this.Equals other = this.Equals other

[<System.Obsolete>]
[<RequireQualifiedAccess>]
module CustomAttributeParent =
    let Assembly (assem: DefinedAssembly) = CustomAttributeParent assem
