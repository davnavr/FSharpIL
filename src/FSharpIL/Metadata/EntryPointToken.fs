namespace FSharpIL.Metadata

open FSharpIL.Metadata.Tables

/// <summary>
/// Represents an <c>EntryPointToken</c>, which specifies the <c>MethodDef</c> or <c>File</c> that is the entry point
/// (II.25.3.3).
/// </summary>
[<System.Runtime.CompilerServices.IsReadOnly>]
type EntryPointToken = struct
    val Token: MetadataToken
    private new (token) = { Token = token }
    new (index: TableIndex<MethodDefRow>) = { Token = MetadataToken(MetadataTokenType.MethodDef, index.TableIndex) }
    new (index: TableIndex<FileRow>) = { Token = MetadataToken(MetadataTokenType.File, index.TableIndex) }
    member this.IsMethodDef = this.Token.Type = MetadataTokenType.MethodDef
    member this.IsFile = this.Token.Type = MetadataTokenType.File
    member this.IsNull = this.Token.IsNull
    static member op_Implicit(token: EntryPointToken) = uint32 token.Token
end

(*
[<RequireQualifiedAccess>]
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type EntryPointToken =
    | MethodDef of TableIndex<MethodDefRow>
    | File of TableIndex<FileRow>
    | Null
*)

[<RequireQualifiedAccess>]
module EntryPointToken =
    let inline MethodDef (index: TableIndex<MethodDefRow>) = EntryPointToken index
    let inline File (index: TableIndex<FileRow>) = EntryPointToken index
    let Null = EntryPointToken()

    let tryOfToken token =
        match token with
        | MetadataToken.Null -> Ok Null
        | MetadataToken.Token(MetadataTokenType.MethodDef, index) -> Ok(MethodDef { TableIndex = index })
        | MetadataToken.Token(MetadataTokenType.File, index) -> Ok(File { TableIndex = index })
        | MetadataToken.Token(table, _) -> Error table

    let tryOfInt value = tryOfToken(MetadataToken value)
