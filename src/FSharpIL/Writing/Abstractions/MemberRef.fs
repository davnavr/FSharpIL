namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

type [<IsReadOnly; Struct; RequireQualifiedAccess>] InstanceMethodRef = { Index: TableIndex<MemberRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] StaticMethodRef = { Index: TableIndex<MemberRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] ObjectConstructorRef = { Index: TableIndex<MemberRefRow> }

type [<IsReadOnly; Struct; RequireQualifiedAccess>] InstanceFieldRef = { Index: TableIndex<MemberRefRow> }
type [<IsReadOnly; Struct; RequireQualifiedAccess>] StaticFieldRef = { Index: TableIndex<MemberRefRow> }
