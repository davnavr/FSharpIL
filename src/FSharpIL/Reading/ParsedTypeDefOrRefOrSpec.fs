namespace FSharpIL.Reading

open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedTypeDefOrRefOrSpec = private { Tag: TypeDefOrRefOrSpecTag; TypeIndex: uint32 }

[<RequireQualifiedAccess>]
module ParsedTypeDefOrRefOrSpec =
    let (|TypeDef|TypeRef|TypeSpec|Unknown|) { Tag = tag; TypeIndex = i } =
        match tag with
        | TypeDefOrRefOrSpecTag.Def -> TypeDef i
        | TypeDefOrRefOrSpecTag.Ref -> TypeRef i
        | TypeDefOrRefOrSpecTag.Spec -> TypeSpec i
        | _ -> Unknown(tag, i)
