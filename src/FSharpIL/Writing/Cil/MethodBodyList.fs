namespace FSharpIL.Writing.Cil

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyList internal () =
    let bodies = ImmutableArray.CreateBuilder<MethodBodyBuilder>()

    member _.Add(maxStack, localVarSig, offset: outref<MethodBodyLocation>) =
        let i = bodies.Count
        offset <- MethodBodyLocation(uint32 i)
        bodies.Add(failwith "method body")
        &Unsafe.AsRef(&bodies.ItemRef i)

    /// Gets the number of method bodies.
    member _.Count = bodies.Count
