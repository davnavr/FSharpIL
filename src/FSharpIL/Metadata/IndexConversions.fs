namespace global

open System.Runtime.CompilerServices

open FSharpIL.Metadata

/// Contains static methods for converting between indices of different types.
[<AbstractClass; Sealed; Extension>]
type IndexConversions =
    [<Extension>] static member inline private ToTypeIndex(index: RawIndex<_>) = RawIndex<TypeDefRow> index.Value
    [<Extension>] static member AsTypeIndex(index: RawIndex<ClassDef<_>>) = index.ToTypeIndex()
    [<Extension>] static member AsTypeIndex(index: RawIndex<StructDef>) = index.ToTypeIndex()
    [<Extension>] static member AsMethodIndex(index: RawIndex<Method<_, _, _>>) = RawIndex<MethodDefRow> index.Value
    [<Extension>] static member AsFieldIndex(index: RawIndex<Field<_>>) = RawIndex<FieldRow> index.Value
