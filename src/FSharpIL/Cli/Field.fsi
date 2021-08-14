namespace FSharpIL.Cli

open System

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<Class>]
type Field =
    val Name: Identifier
    val Type: CliType

    internal new: name: Identifier * fieldType: CliType -> Field

    abstract Equals: other: Field -> bool
    default Equals: other: Field -> bool

    override ToString: unit -> string
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<Field>

[<AbstractClass>]
type DefinedField =
    inherit Field

    member Flags: FieldFlags

    new: flags: FieldFlags * name: Identifier * fieldType: CliType -> DefinedField

    override Equals: other: Field -> bool

[<Sealed; Class>]
type LiteralFieldDefinition =
    inherit DefinedField

    member Value: Constant

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

[<Sealed; Class>]
type FieldDefinition<'Kind when 'Kind :> IAttributeTag<FieldFlags> and 'Kind : struct> =
    inherit DefinedField

type DefinedField with
    static member Instance:
        visibility: MemberVisibility *
        flags: FieldAttributes<FieldKinds.Instance> *
        name: Identifier *
        signature: CliType -> FieldDefinition<FieldKinds.Instance>

    static member Static:
        visibility: MemberVisibility *
        flags: FieldAttributes<FieldKinds.Static> *
        name: Identifier *
        signature: CliType -> FieldDefinition<FieldKinds.Static>

    //static member Literal // TODO: How to enforce correct field type?
    //static member Rva

[<AbstractClass>]
type ReferencedField =
    inherit Field

    val Visibility: ExternalVisibility

    override Equals: other: Field -> bool

//LiteralFieldReference

[<Sealed; Class>]
type FieldReference<'Kind when 'Kind :> IAttributeTag<FieldFlags>> =
    inherit ReferencedField

[<RequireQualifiedAccess>]
module Field =
    open System.Collections.Generic

    val inline (|Defined|Referenced|): field: Field -> Choice<DefinedField, ReferencedField>

    [<Sealed; Class>]
    type SignatureComparer = interface IEqualityComparer<Field>

    val signatureComparer : SignatureComparer

    [<Sealed; Class>]
    type DefinitionComparer = interface IEqualityComparer<DefinedField>

    val definitionComparer : DefinitionComparer

[<RequireQualifiedAccess>]
module DefinedField =
    val inline (|Instance|Static|Literal|WithRva|):
        field: DefinedField ->
            Choice<FieldDefinition<FieldKinds.Instance>,
                   FieldDefinition<FieldKinds.Static>,
                   LiteralFieldDefinition,
                   RvaFieldDefinition>
