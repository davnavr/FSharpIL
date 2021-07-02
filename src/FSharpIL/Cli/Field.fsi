namespace FSharpIL.Cli

open System

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli.Signatures

[<AbstractClass>]
type Field =
    val Name: Identifier
    val Signature: FieldSig

    member Type: EncodedType

    abstract Equals: other: Field -> bool
    default Equals: other: Field -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<Field>

[<AbstractClass>]
type DefinedField =
    inherit Field

    member Flags: FieldFlags

    override Equals: other: Field -> bool

[<Sealed>]
type LiteralFieldDefinition = class
    inherit DefinedField

    member Value: Constant
end

/// Represents a defined field whose value is stored at the specified Relative Virtual Address (II.16.3.2).
[<Sealed>]
type RvaFieldDefinition =
    inherit DefinedField

    val Value: FSharpIL.ChunkedMemory

[<RequireQualifiedAccess>]
module FieldKinds =
    type [<Struct>] Instance =
        interface IAttributeTag<FieldFlags>

    type [<Struct>] Static =
        interface IAttributeTag<FieldFlags>

[<Sealed>]
type FieldDefinition<'Kind when 'Kind :> IAttributeTag<FieldFlags> and 'Kind : struct> = class
    inherit DefinedField
end

type DefinedField with
    static member Instance:
        visibility: MemberVisibility *
        flags: FieldAttributes<FieldKinds.Instance> *
        name: Identifier *
        signature: FieldSig -> FieldDefinition<FieldKinds.Instance>

    static member Static:
        visibility: MemberVisibility *
        flags: FieldAttributes<FieldKinds.Static> *
        name: Identifier *
        signature: FieldSig -> FieldDefinition<FieldKinds.Static>

    //static member Literal // TODO: How to enforce correct field type?
    //static member Rva

[<AbstractClass>]
type ReferencedField = class
    inherit Field

    val Visibility: ExternalVisibility

    override Equals: other: Field -> bool
end

//LiteralFieldReference

[<Sealed>]
type FieldReference<'Kind when 'Kind :> IAttributeTag<FieldFlags>> = class
    inherit ReferencedField
end

type ReferencedField with
    static member Instance:
        visibility: ExternalVisibility *
        name: Identifier *
        signature: FieldSig -> FieldReference<FieldKinds.Instance>

    static member Static:
        visibility: ExternalVisibility *
        name: Identifier *
        signature: FieldSig -> FieldReference<FieldKinds.Static>

    //static member Literal

[<RequireQualifiedAccess>]
module Field =
    val inline (|Defined|Referenced|): field: Field -> Choice<DefinedField, ReferencedField>

[<RequireQualifiedAccess>]
module DefinedField =
    val inline (|Instance|Static|Literal|WithRva|):
        field: DefinedField ->
            Choice<FieldDefinition<FieldKinds.Instance>,
                   FieldDefinition<FieldKinds.Static>,
                   LiteralFieldDefinition,
                   RvaFieldDefinition>

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type FieldArg = // TODO: Prevent usage of default ctor
    member Owner: FSharpIL.Cli.Type
    member Field: Field

[<RequireQualifiedAccess>]
module FieldArg =
    val Defined: DefinedType * DefinedField -> FieldArg

    val Referenced: ReferencedType * ReferencedField -> FieldArg

    val inline (|Defined|Referenced|):
        field: FieldArg -> Choice<struct(DefinedType * DefinedField), struct(ReferencedType * ReferencedField)>
