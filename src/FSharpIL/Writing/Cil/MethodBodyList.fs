namespace FSharpIL.Writing.Cil

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata.Tables

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyList internal () =
    let [<Literal>] BodyChunkSize = 64
    let bodies = ImmutableArray.CreateBuilder<MethodBodyBuilder>()

    member _.Add(maxStack, localVarSig, offset: outref<MethodBodyLocation>) =
        let i = bodies.Count
        offset <- MethodBodyLocation(uint32 i)
        bodies.Add
            { MethodBody = ChunkedMemoryBuilder BodyChunkSize
              BranchTargets = { Targets = ImmutableArray.CreateBuilder() }
              MaxStack = maxStack
              LocalVarSigTok = localVarSig }
        &Unsafe.AsRef(&bodies.ItemRef i)

    /// Gets the number of method bodies.
    member _.Count = bodies.Count

    member internal _.Serialize(wr: byref<ChunkedMemoryBuilder>) =
        for i = 0 to bodies.Count - 1 do
            MethodBodyBuilder.serialize (&bodies.ItemRef i) &wr
