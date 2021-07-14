namespace FSharpIL.Cli

open System

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<AbstractClass>]
type Field =
    val Name: Identifier
    val Type: NamedType

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
    member Owner: NamedType
    member Field: Field

[<RequireQualifiedAccess>]
module FieldArg =
    val Defined: DefinedType * DefinedField -> FieldArg

    val Referenced: ReferencedType * ReferencedField -> FieldArg

    //val Specification

    val inline (|Defined|Referenced|):
        field: FieldArg -> Choice<struct(DefinedType * DefinedField), struct(ReferencedType * ReferencedField)>
