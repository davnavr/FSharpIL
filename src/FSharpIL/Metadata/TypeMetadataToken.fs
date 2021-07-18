namespace FSharpIL.Metadata

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeMetadataToken private (token: MetadataToken) = struct
    internal new (tag, index) = TypeMetadataToken(MetadataToken(tag, index))
    member _.Type = token.Type
    member _.Token = token
    override _.ToString() = token.ToString()
end

[<RequireQualifiedAccess>]
module TypeMetadataToken =
    let Def ({ TableIndex = i }: TableIndex<TypeDefRow>) = TypeMetadataToken(MetadataTokenType.TypeDef, i)
    let Ref ({ TableIndex = i }: TableIndex<TypeRefRow>) = TypeMetadataToken(MetadataTokenType.TypeRef, i)
    let Spec ({ TableIndex = i }: TableIndex<TypeSpecRow>) = TypeMetadataToken(MetadataTokenType.TypeSpec, i)

    let ofCodedIndex (index: TypeDefOrRef) =
        let tag =
            match index.Tag with
            | TypeDefOrRefTag.TypeDef -> MetadataTokenType.TypeDef
            | TypeDefOrRefTag.TypeRef -> MetadataTokenType.TypeRef
            | TypeDefOrRefTag.TypeSpec
            | _ -> MetadataTokenType.TypeSpec
        TypeMetadataToken(tag, index.Index)
