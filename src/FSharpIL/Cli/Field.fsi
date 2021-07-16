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

    new: flags: FieldFlags * name: Identifier * fieldType: NamedType -> DefinedField

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

    [<Sealed>]
    type SignatureComparer = class
        interface System.Collections.Generic.IEqualityComparer<Field>
    end

    val signatureComparer : SignatureComparer

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
type FieldArg<'Owner, 'Field when 'Owner :> NamedType and 'Field :> Field> =
    member Owner: 'Owner
    member Field: 'Field

    internal new: owner: 'Owner * field: 'Field -> FieldArg<'Owner, 'Field>

type FieldArg = FieldArg<NamedType, Field>

[<AutoOpen>]
module FieldArgPatterns =
    val inline (|FieldArg|) : field: FieldArg<'Owner, 'Field> -> struct('Owner * 'Field)
