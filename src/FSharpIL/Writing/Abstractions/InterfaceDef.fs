namespace FSharpIL.Writing.Abstractions

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type InterfaceDef =
    { Access: TypeVisibility
      InterfaceName: Identifier
      Namespace: string }
    interface ITableRow

[<RequireQualifiedAccess>]
module Interface =
    let typeIndex ({ TableIndex = index }: TableIndex<InterfaceDef>): TableIndex<TypeDefRow> = { TableIndex = index }

