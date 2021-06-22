namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing
open FSharpIL.Writing.Tables

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

type ClassDef<'Kind when 'Kind :> TypeKinds.Kind> =
    { Access: TypeVisibility
      Flags: TypeDefFlags // TODO: Create custom flag type.
      ClassName: Identifier
      Namespace: Identifier voption
      Extends: ClassExtends }

    interface ITableRow

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends(ClassExtendsTag.Null, 0u)

    let toCodedIndex (extends: ClassExtends) =
        match extends.Tag with
        | ClassExtendsTag.Null
        | _ -> TypeDefOrRef()

type ConcreteClassDef = ClassDef<TypeKinds.ConcreteClass>
type AbstractClassDef = ClassDef<TypeKinds.AbstractClass>
type SealedClassDef = ClassDef<TypeKinds.SealedClass>
type StaticClassDef = ClassDef<TypeKinds.StaticClass>

[<RequireQualifiedAccess>]
module internal ClassDef =
    let add (row: inref<ClassDef<'Kind>>) parent builder = // TODO: Make MemberOwner kind for types that can contain other types?
        let entry =
            { Flags = invalidOp "flags?"
              TypeName = builder.Metadata.Strings.Add row.ClassName
              TypeNamespace = builder.Strings.Add row.Namespace
              Extends = ClassExtends.toCodedIndex row.Extends
              EnclosingClass = parent }
        ModuleBuilder.addTypeEntry &entry builder

module StaticClass =
    let add (row: inref<StaticClassDef>) parent builder = ClassDef.add &row parent builder
