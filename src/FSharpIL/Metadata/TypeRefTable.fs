namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.ComponentModel

[<StructuralComparison; StructuralEquality>]
type TypeRefToken =
    { // ResolutionScope
      TypeName: string
      TypeNamespace: string }

[<NoComparison; NoEquality>]
type TypeRef = Temporary

[<RequireQualifiedAccess>]
module TypeRefTable =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Table =
        private
        | Table of ImmutableSortedDictionary<TypeRefToken, TypeRef>

    let empty = Table ImmutableSortedDictionary.Empty

// II.22.38
type TypeRefTable = TypeRefTable.Table
