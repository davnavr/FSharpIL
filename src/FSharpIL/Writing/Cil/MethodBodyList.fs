namespace FSharpIL.Writing.Cil

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities

[<IsReadOnly; Struct>]
type MethodBodyHeader =
    { InitLocals: InitLocals
      MaxStack: uint16
      LocalVariables: TableIndex<StandaloneSigRow> }

[<Struct>]
type internal MethodBody =
    { Header: MethodBodyHeader
      Instructions: ChunkedMemoryBuilder }

    member inline this.CodeSize = this.Instructions.Length

[<RequireQualifiedAccess>]
module internal MethodBody =
    let [<Literal>] MaxTinyBodySize = 63u
    let [<Literal>] MaxTinyMaxStack = 8us
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    let [<Literal>] FatFormatSize = 3u
    let [<Literal>] FatMethodAlignment = 4u

    let flags (body: inref<MethodBody>) =
        let mutable flags = ILMethodFlags.None

        //if exceptions present
        //if extra data sections present

        if
            flags = ILMethodFlags.None
            && body.CodeSize <= MaxTinyBodySize
            && body.Header.LocalVariables.IsNull
            && body.Header.MaxStack <= MaxTinyMaxStack
        then
                flags <- ILMethodFlags.TinyFormat
            else
                flags <- flags ||| ILMethodFlags.FatFormat

        flags

type IMethodBodySource = interface
    abstract Create: byref<MethodBodyBuilder> -> MethodBodyHeader
end

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyList internal () =
    static let [<Literal>] BodyChunkSize = 64 // TODO: Figure out how to allow reuse of method body chunks.
    let bodies = ImmutableArray.CreateBuilder<MethodBody>()
    let mutable offset = 0u

    member _.Add(source: inref<#IMethodBodySource>) =
        let start = offset
        let mutable builder =
            { branchTargetList = ImmutableArray.CreateBuilder()
              estimatedMaxStack = 0us
              instructions = ChunkedMemoryBuilder BodyChunkSize }
        let header = source.Create &builder

        if builder.branchTargetList.Count > 0 then noImpl "TODO: How to write branch targets without rewriting method body a total of three times (initial write, rewrite here, and serialization)"

        let bodyi = bodies.Count

        bodies.Add { Header = header; Instructions = builder.instructions }

        let body = &bodies.ItemRef bodyi // Possible struct copy since MethodBody is not immutable
        let isTinyMethod = MethodBody.flags &body = ILMethodFlags.TinyFormat
        let methodHeaderSize = if isTinyMethod then 1u else (MethodBody.FatFormatSize * 4u)
        let padding = if isTinyMethod then 0u else (Round.upTo 4u offset) - offset

        offset <- offset + body.CodeSize + methodHeaderSize + padding
        MethodBodyLocation(start + padding)

    member internal _.Serialize(wr: byref<ChunkedMemoryBuilder>) =
        for i = 0 to bodies.Count - 1 do
            let body = &bodies.ItemRef i // Possible struct copy since MethodBody is not immutable
            let flags = MethodBody.flags &body
            if flags = ILMethodFlags.TinyFormat then
                wr.Write(uint8 flags ||| (Checked.uint8 body.CodeSize <<< 2))
            else
                let mutable flags' = ILMethodFlags.FatFormat

                match body.Header.InitLocals with
                | InitLocals -> flags' <- flags' ||| ILMethodFlags.InitLocals
                | SkipInitLocals -> ()

                //if has extra data sections then flags' <- flags' ||| ILMethodFlags.MoreSects

                wr.AlignTo(int32 MethodBody.FatMethodAlignment) // Padding
                wr.WriteLE(uint16 flags' ||| (uint16 MethodBody.FatFormatSize <<< 12)) // Flags & size
                wr.WriteLE body.Header.MaxStack
                wr.WriteLE body.CodeSize
                wr.WriteLE(uint32(MetadataToken(MetadataTokenType.StandaloneSig, body.Header.LocalVariables.TableIndex)))
                // TODO: Write extra data sections for fat method.
            wr.Write(body.Instructions.ToImmutable())
