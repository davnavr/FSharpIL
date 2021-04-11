namespace global

open System.Runtime.CompilerServices

open FSharpIL.Metadata

/// Contains static methods for converting between indices of different types.
[<System.Obsolete>]
[<AbstractClass; Sealed; Extension>]
type IndexConversions =
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member inline private ToTypeIndex(index: RawIndex<_>) = RawIndex<TypeDefRow> index.Value
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsTypeIndex(index: RawIndex<ClassDef<_>>) = index.ToTypeIndex()
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsTypeIndex(index: RawIndex<DelegateDef>) = index.ToTypeIndex()
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsTypeIndex(index: RawIndex<StructDef>) = index.ToTypeIndex()
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsMethodIndex(index: RawIndex<Method<_, _, _>>) = RawIndex<MethodDefRow> index.Value
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsMethodIndex(index: RawIndex<Constructor<_, _>>) = RawIndex<MethodDefRow> index.Value
    [<Extension; System.Obsolete("Use index conversion functions in modules instead")>] static member AsFieldIndex(index: RawIndex<Field<_>>) = RawIndex<FieldRow> index.Value
