namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing

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

type ClassDef<'Kind> =
    { Access: TypeVisibility
      Flags: TypeDefFlags // TODO: Create custom flag type.
      ClassName: Identifier
      Namespace: string
      Extends: ClassExtends }

    interface ITableRow

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends(ClassExtendsTag.Null, 0u)

    let toCodedIndex (extends: ClassExtends) =
        match extends.Tag with
        | ClassExtendsTag.Null
        | _ -> TypeDefOrRef()

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
module private ClassDef =
    let tryAddRow (builder: CliMetadataBuilder) (row: ClassDef<'Kind>) =
        let row' =
            { Flags = invalidOp "flags?"
              TypeName = builder.Strings.GetOrAdd row.ClassName
              TypeNamespace =
                match Identifier.tryOfStr row.Namespace with
                | ValueSome typeNamespace -> builder.Strings.GetOrAdd typeNamespace
                | ValueNone -> Unchecked.defaultof<_>
              Extends = ClassExtends.toCodedIndex row.Extends
              FieldList = invalidOp "get fields?"
              MethodList = invalidOp "get methods?" }
        ()
