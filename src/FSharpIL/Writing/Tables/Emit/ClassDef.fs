namespace FSharpIL.Writing.Tables.Emit

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type ClassExtendsTag =
    | Null = 0uy
    | ConcreteClassDef = 1uy
    | AbstractClassDef = 2uy
    | ConcreteClassRef = 3uy
    | AbstractClassRef = 4uy
    | TypeSpec = 5uy

[<IsReadOnly; Struct>]
type ClassDef = struct
    val Access: TypeVisibility
    val Flags: TypeDefFlags
    val ClassName: Identifier
    val Namespace: string
    val Extends: ClassExtends
end

and [<IsReadOnly; Struct>] ClassExtends =
    val Tag: ClassExtendsTag
    val Index: uint32
    internal new (tag, index) = { Tag = tag; Index = index }
    member this.IsNull = this.Tag = ClassExtendsTag.Null

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends(ClassExtendsTag.Null, 0u)

[<IsReadOnly>]
type ConcreteClassDef = struct
    val Definition: ClassDef
    interface ITableRow
end

[<IsReadOnly>]
type AbstractClassDef= struct
   val Definition: ClassDef
   interface ITableRow
end

[<IsReadOnly>]
type SealedClassDef= struct
   val Definition: ClassDef
   interface ITableRow
end

[<IsReadOnly>]
type StaticClassDef= struct
   val Definition: ClassDef
   interface ITableRow
end

[<RequireQualifiedAccess>]
module ConcreteClass =
    let typeIndex ({ TableIndex = index }: TableIndex<ConcreteClassDef>): TableIndex<TypeDefRow> = { TableIndex = index }

[<RequireQualifiedAccess>]
module AbstractClass =
    let typeIndex ({ TableIndex = index }: TableIndex<AbstractClassDef>): TableIndex<TypeDefRow> = { TableIndex = index }

[<RequireQualifiedAccess>]
module SealedClass =
    let typeIndex ({ TableIndex = index }: TableIndex<SealedClassDef>): TableIndex<TypeDefRow> = { TableIndex = index }

[<RequireQualifiedAccess>]
module StaticClass =
    let typeIndex ({ TableIndex = index }: TableIndex<StaticClassDef>): TableIndex<TypeDefRow> = { TableIndex = index }
