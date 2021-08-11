namespace FSharpIL.Metadata

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type MethodMetadataToken private (token: MetadataToken) = struct
    internal new (tag, index) = MethodMetadataToken(MetadataToken(tag, index))
    member _.Type = token.Type
    member _.Token = token
    override _.ToString() = token.ToString()
    static member inline op_Implicit(token: MethodMetadataToken) = uint32 token.Token
end

[<RequireQualifiedAccess>]
module MethodMetadataToken =
    let Def ({ TableIndex = i }: TableIndex<MethodDefRow>) = MethodMetadataToken(MetadataTokenType.MethodDef, i)
    let Ref ({ TableIndex = i }: TableIndex<MemberRefRow>) = MethodMetadataToken(MetadataTokenType.MemberRef, i)
    let Spec ({ TableIndex = i }: TableIndex<MethodSpecRow>) = MethodMetadataToken(MetadataTokenType.MethodSpec, i)
