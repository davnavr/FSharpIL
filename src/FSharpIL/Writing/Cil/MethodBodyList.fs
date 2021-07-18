namespace FSharpIL.Writing.Cil

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
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
        let start = MethodBodyLocation offset
        let mutable builder =
            { branchTargetList = ImmutableArray.CreateBuilder()
              estimatedMaxStack = 0us
              instructions = ChunkedMemoryBuilder BodyChunkSize }
        let header = source.Create &builder

        if builder.branchTargetList.Count > 0 then noImpl "TODO: How to write branch targets without rewriting method body a total of three times (initial write, rewrite here, and serialization)"

        let bodyi = bodies.Count
        bodies.Add { Header = header; Instructions = builder.instructions }
        let body = &bodies.ItemRef bodyi

        let methodHeaderSize = if MethodBody.flags &body = ILMethodFlags.TinyFormat then 1u else (MethodBody.FatFormatSize * 4u)
        offset <- offset + body.CodeSize + methodHeaderSize
        start

    /// Gets the number of method bodies.
    member _.Count = bodies.Count

    member internal _.Serialize(wr: byref<ChunkedMemoryBuilder>) =
        for i = 0 to bodies.Count - 1 do
            let body = &bodies.ItemRef i
            let flags = MethodBody.flags &body
            if flags = ILMethodFlags.TinyFormat then
                wr.Write(uint8 flags ||| (Checked.uint8 body.CodeSize <<< 2))
            else
                noImpl "TODO: Write fat format see https://github.com/davnavr/FSharpIL/blob/cd0141ba7c76a709bb9300234ae34aa2567eb1f9/src/FSharpIL/Writing/Cil/MethodBodyBuilder.fs#L161"
            wr.Write(body.Instructions.ToImmutable())
