namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

type [<IsReadOnly; Struct; RequireQualifiedAccess>] ConcreteClassRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] AbstractClassRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] SealedClassRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] StaticClassRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] DelegateRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] EnumRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] InterfaceRef = { Index: TableIndex<TypeRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] ValueTypeRef = { Index: TableIndex<TypeRefRow> }
