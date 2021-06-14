namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
type LiteralField =
    { Visibility: MemberVisibility
      FieldName: Identifier
      FieldType: unit // TODO: Make type for literal field type and literal field value.
      LiteralValue: unit }

    interface ITableRow

[<RequireQualifiedAccess>]
module FieldKinds =
    type Instance = struct end
    type Static = struct end

[<IsReadOnly; Struct>]
type Field<'Kind> =
    { Visibility: MemberVisibility
      FieldName: Identifier
      FieldType: EncodedType }

    interface ITableRow

type InstanceFieldDef = Field<FieldKinds.Instance>
type StaticFieldDef = Field<FieldKinds.Static>
