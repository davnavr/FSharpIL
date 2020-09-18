namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.ComponentModel

// TODO: How to ensure uniqueness of TypeDef and that they are from the same TypeDefTable?
[<StructuralComparison; StructuralEquality>]
type TypeDefToken = // TODO: Maybe move this type inside of the table module.
    { TypeName: string
      TypeNamespace: string }

// II.22.37
[<NoComparison; NoEquality>]
type TypeDef = // TODO: Rename to TypeDefInfo and create a new TypeDef class or record that somehow keeps track of what TypeDefTable its attached to.
    | Class // of
    | Interface // of
    // ValueType // NOTE: Class can just derive from System.ValueType, but it won't be safe since user might not mark it as sealed.

    member this.TypeName = invalidOp "bad name": string
    member this.TypeNamespace = invalidOp "bad namespace": string

[<RequireQualifiedAccess>]
module TypeDefTable =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Table =
        private
        | Table of ImmutableSortedDictionary<TypeDefToken, TypeDef>

    let empty = Table ImmutableSortedDictionary.Empty

    let addTypes (types: TypeDef list) table =
        let (Table state) = table
        let rec inner (state: ImmutableSortedDictionary<_, _>) = // TODO: Check if this is tail recursive.
            function
            | [] -> Table state |> Ok
            | (thead: TypeDef) :: ttail ->
                let token =
                    { TypeName = thead.TypeName
                      TypeNamespace = thead.TypeNamespace }
                match state.TryGetValue token with
                | (true, err) -> Error err
                | (false, _) ->
                    let state' = state.Add(token, thead)
                    inner state' ttail
        inner state types

// II.22.37
type TypeDefTable = TypeDefTable.Table
