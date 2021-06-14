namespace FSharpIL.Writing.Abstractions

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

type [<IsReadOnly; Struct>] ClassExtends =
    val Tag: ClassExtendsTag
    val Index: uint32
    internal new (tag, index) = { Tag = tag; Index = index }
    member this.IsNull = this.Tag = ClassExtendsTag.Null

[<IsReadOnly; Struct>]
type ClassDef<'Kind> =
    { Access: TypeVisibility
      Flags: TypeDefFlags
      ClassName: Identifier
      Namespace: string
      Extends: ClassExtends }

    interface ITableRow

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends(ClassExtendsTag.Null, 0u)

[<RequireQualifiedAccess>]
module ClassKinds =
    type Concrete = struct end
    type Abstract = struct end
    type Sealed = struct end
    type Static = struct end

type ConcreteClassDef = ClassDef<ClassKinds.Concrete>
type AbstractClassDef = ClassDef<ClassKinds.Abstract>
type SealedClassDef = ClassDef<ClassKinds.Sealed>
type StaticClassDef = ClassDef<ClassKinds.Static>

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
