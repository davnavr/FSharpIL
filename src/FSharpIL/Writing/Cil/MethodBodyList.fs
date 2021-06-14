namespace FSharpIL.Writing.Cil

open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata.Tables

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyList internal () =
    static let [<Literal>] BodyChunkSize = 64
    let bodies = List<MethodBodyBuilder ref>()

    member _.Add(maxStack, localVarSig, offset: outref<MethodBodyLocation>) =
        let i = bodies.Count
        offset <- MethodBodyLocation(uint32 i)
        let body =
            { MethodBody = ChunkedMemoryBuilder BodyChunkSize
              BranchTargets = { Targets = ImmutableArray.CreateBuilder() }
              MaxStack = maxStack
              LocalVarSigTok = localVarSig }
            |> ref
        bodies.Add body
        &body.contents

    /// Gets the number of method bodies.
    member _.Count = bodies.Count

    member internal _.Serialize(wr: byref<ChunkedMemoryBuilder>) =
        for body in bodies do
            MethodBodyBuilder.serialize (&body.contents) &wr
