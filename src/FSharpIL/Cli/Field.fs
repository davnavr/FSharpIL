namespace FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli.Signatures

[<AbstractClass>]
type Field =
    val Name: Identifier
    val Signature: FieldSig

    new (name, signature) = { Name = name; Signature = signature }

    member this.Type = this.Signature.FieldType

    abstract Equals: other: Field -> bool
    default this.Equals(other: Field) = this.Name = other.Name && this.Type = other.Type

    override this.Equals(obj: obj) =
        match obj with
        | :? Field as other -> this.Equals(other = other)
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Signature)

    interface IEquatable<Field> with member this.Equals other = this.Equals(other = other)

[<AbstractClass>]
type DefinedField (flags: FieldFlags, name, signature) =
    inherit Field(name, signature)

    member _.Flags = flags

    override _.Equals(other: Field) =
        match other with
        | :? DefinedField as other' ->
            (flags ||| other'.Flags &&& FieldFlags.FieldAccessMask <> FieldFlags.CompilerControlled)
            && base.Equals(other = other)
        | _ -> false

[<Sealed>]
type LiteralFieldDefinition (name, signature, value: Constant) =
    inherit DefinedField(FieldFlags.Static ||| FieldFlags.Literal, name, signature)

    member _.Value = value

[<Sealed>]
type RvaFieldDefinition =
    inherit DefinedField

    val Value: FSharpIL.ChunkedMemory

    new (name, signature, value: FSharpIL.ChunkedMemory) =
        { inherit DefinedField(FieldFlags.Static ||| FieldFlags.HasFieldRva, name, signature)
          Value = value }

[<RequireQualifiedAccess>]
module FieldKinds =
    type Instance = struct
        interface IAttributeTag<FieldFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>
    end

    type Static = struct
        interface IAttributeTag<FieldFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = FieldFlags.Static
    end

[<Sealed>]
type FieldDefinition<'Kind when 'Kind :> IAttributeTag<FieldFlags> and 'Kind : struct>
    (visibility, flags: FieldAttributes<'Kind>, name, signature)
    =
    inherit DefinedField (
        Unchecked.defaultof<'Kind>.RequiredFlags ||| MemberVisibility.ofField visibility ||| flags.Flags,
        name,
        signature
    )

type DefinedField with
    static member Instance(visibility, flags, name, signature) =
        FieldDefinition<FieldKinds.Instance>(visibility, flags, name, signature)

    static member Static(visibility, flags, name, signature) =
        FieldDefinition<FieldKinds.Static>(visibility, flags, name, signature)

[<AbstractClass>]
type ReferencedField =
    inherit Field

    val Visibility: ExternalVisibility

    new (visibility, name, signature) =
        { inherit Field(name, signature)
          Visibility = visibility }

    override _.Equals(other: Field) =
        match other with
        | :? ReferencedField -> base.Equals(other = other)
        | _ -> false

[<Sealed>]
type FieldReference<'Kind when 'Kind :> IAttributeTag<FieldFlags>> (visibility, name, signature) =
    inherit ReferencedField(visibility, name, signature)

type ReferencedField with
    static member Instance(visibility, name, signature) = FieldReference<FieldKinds.Instance>(visibility, name, signature)
    static member Static(visibility, name, signature) = FieldReference<FieldKinds.Static>(visibility, name, signature)

[<RequireQualifiedAccess>]
module Field =
    let inline (|Defined|Referenced|) (field: Field) =
        match field with
        | :? DefinedField as fdef -> Defined fdef
        | _ -> Referenced(field :?> ReferencedField)

[<RequireQualifiedAccess>]
module DefinedField =
    let inline (|Instance|Static|Literal|WithRva|) (field: DefinedField) =
        match field with
        | :? FieldDefinition<FieldKinds.Instance> as ifield -> Instance ifield
        | :? FieldDefinition<FieldKinds.Static> as sfield -> Static sfield
        | :? LiteralFieldDefinition as lfield -> Literal lfield
        | _ -> WithRva(field :?> RvaFieldDefinition)

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type FieldArg (owner: FSharpIL.Cli.Type, field: Field) =
    member _.Owner = owner
    member _.Field = field

[<RequireQualifiedAccess>]
module FieldArg =
    let Defined (owner: DefinedType, field: DefinedField) = FieldArg(owner, field)
    let Referenced (owner: ReferencedType, field: ReferencedField) = FieldArg(owner, field)

    let inline (|Defined|Referenced|) (field: FieldArg) =
        match field.Owner with
        | :? DefinedType as tdef -> Defined(struct(tdef, field.Field :?> DefinedField))
        | _ -> Referenced(struct(field.Owner :?> ReferencedType, field.Field :?> ReferencedField))
