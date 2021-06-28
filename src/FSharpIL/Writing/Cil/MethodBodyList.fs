namespace FSharpIL.Writing.Cil

open System.Collections.Generic
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

type IMethodBodySource = interface
    abstract Create: byref<MethodBodyBuilder> -> MethodBodyHeader
end

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyList internal () =
    static let [<Literal>] BodyChunkSize = 128
    let bodies = List<MethodBody>()
    let mutable offset = 0u

    member _.Add(source: #IMethodBodySource) =
        let start = MethodBodyLocation offset
        let mutable builder =
            { estimatedMaxStack = 0us
              branchTargetList = ImmutableArray.CreateBuilder()
              // TODO: Consider having 1 shared ChunkedMemoryBuilder as a mutable instance field instead, since the offset field is instance and mutable anyway.
              methodBody = ChunkedMemoryBuilder BodyChunkSize }
        let header = source.Create &builder

        if builder.branchTargetList.Count > 0 then noImpl "TODO: How to write branch targets without rewriting method body a total of three times (initial write, rewrite here, and serialization)"
        let body = MethodBody(builder.methodBody.ToImmutable(), header.MaxStack, header.LocalVariables, header.InitLocals)

        let flags = body.Flags
        offset <- offset + body.CodeSize + if flags = ILMethodFlags.TinyFormat then 1u else (MethodBody.FatFormatSize * 4u)
        // TODO: Loop through extra data sections and add the sizes.
        bodies.Add body
        start

    /// Gets the number of method bodies.
    member _.Count = bodies.Count

    member internal _.Serialize(wr: byref<ChunkedMemoryBuilder>) =
        for body in bodies do
            let flags = body.Flags
            if flags = ILMethodFlags.TinyFormat then
                wr.Write(uint8 ILMethodFlags.TinyFormat ||| Checked.uint8 body.CodeSize)
            else
                noImpl "TODO: Write fat format see https://github.com/davnavr/FSharpIL/blob/cd0141ba7c76a709bb9300234ae34aa2567eb1f9/src/FSharpIL/Writing/Cil/MethodBodyBuilder.fs#L161"
            wr.Write body.InstructionStream
