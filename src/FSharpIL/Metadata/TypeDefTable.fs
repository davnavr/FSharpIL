namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.ComponentModel

open FSharpIL.Utilities

// II.22.37
type TypeDef = // TODO: Rename to TypeDefInfo and create a new TypeDef class or record that somehow keeps track of what TypeDefTable its attached to.
    | Class
    | Interface
    // ValueType // NOTE: Class can just derive from System.ValueType.

    // member this.Token = 

// type TypeDefToken = 

[<RequireQualifiedAccess>]
module TypeDefTable =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Table =
        private
        | Table of ImmutableSortedSet<TypeDef>

    let empty =
        ImmSet.empty
        |> ImmSet.withComparer
            (invalidOp "bad comparer")
        |> Table

// II.22.37
type TypeDefTable = TypeDefTable.Table
