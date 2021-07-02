namespace FSharpIL.Metadata

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type FieldMetadataToken private (token: MetadataToken) = struct
    internal new (tag, index) = FieldMetadataToken(MetadataToken(tag, index))
    member _.Type = token.Type
    member _.Token = token
    override _.ToString() = token.ToString()
end

[<RequireQualifiedAccess>]
module FieldMetadataToken =
    let Def ({ TableIndex = i }: TableIndex<FieldRow>) = FieldMetadataToken(MetadataTokenType.Field, i)
    let Ref ({ TableIndex = i }: TableIndex<MemberRefRow>) = FieldMetadataToken(MetadataTokenType.MemberRef, i)
