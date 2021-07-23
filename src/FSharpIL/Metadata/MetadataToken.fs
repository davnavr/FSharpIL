namespace FSharpIL.Metadata

open FSharpIL.Utilities

type MetadataTokenType =
    | Module = 0uy
    | TypeRef = 1uy
    | TypeDef = 2uy
    | Field = 4uy
    | MethodDef = 6uy
    | MemberRef = 0xAuy
    | StandaloneSig = 0x11uy
    | TypeSpec = 0x1Buy
    | File = 0x26uy
    | MethodSpec = 0x2Buy
    | UserStringHeap = 0x70uy

/// <summary>
/// Represents a metadata token (III.1.9).
/// </summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type MetadataToken internal (value: uint32) =
    static let [<Literal>] IndexMask = 0xFFFFFFu

    internal new (tag: MetadataTokenType, index) =
        if index > IndexMask then argOutOfRange "index" index "The index must be able to fit into 3 bytes"
        MetadataToken((uint32 tag <<< 24) ||| index)

    member _.Index = value &&& IndexMask
    member _.Type = LanguagePrimitives.EnumOfValue<_, MetadataTokenType>(uint8(value >>> 24))
    member _.IsNull = value = 0u
    member _.Value = value

    static member op_Implicit(token: MetadataToken) = token.Value

    override this.ToString() =
        if this.IsNull
        then System.String.Empty
        else sprintf "%A (0x%08X)" this.Type this.Index

[<RequireQualifiedAccess>]
module MetadataToken =
    let inline (|Token|Null|) (token: MetadataToken) =
        if token.IsNull
        then Null
        else Token(struct(token.Type, token.Index))
