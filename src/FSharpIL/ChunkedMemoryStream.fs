namespace FSharpIL

open System
open System.IO

open FSharpIL.Utilities

/// A read-only sequence of bytes that supports seeking used to read a non-contiguous region of memory.
[<Sealed>]
type ChunkedMemoryStream (data: ChunkedMemory) =
    inherit Stream()
    let mutable pos = 0u

    override _.CanRead = true
    override _.CanSeek = true
    override _.CanWrite = false
    override _.Length = int64 data.Length

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the new position is negative or exceeds the length of the stream.
    /// </exception>
    override this.Position
        with get() = int64 pos
        and set position =
            if position < 0L || position > this.Length then argOutOfRange "position" position "The position was out of bounds"
            pos <- uint32 position
    
    override _.Flush() = ()

    override _.Read(buffer, offset, count) =
        let count' = int32(min data.Length (uint32 count))
        if count' > 0 then
            data.UnsafeCopyTo(pos, Span(buffer, offset, count'))
            pos <- pos + uint32 count
        count'

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="origin"/> is invalid.
    /// </exception>
    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown when seeking is attempted before the start or past the end of the stream.
    /// </exception>
    override this.Seek(offset, origin) =
        let pos' =
            match origin with
            | SeekOrigin.Begin -> offset
            | SeekOrigin.Current -> int64 pos + offset
            | SeekOrigin.End -> this.Length + offset
            | _ -> argOutOfRange "origin" origin "The origin is invalid"
        if pos' < 0L then invalidOp "Cannot seek before the start of the stream"
        if pos' >= this.Length then invalidOp "Cannot seek past the end of the stream"
        pos <- uint32 pos'
        this.Position

    override _.SetLength _ = notSupported "Cannot set the length, the stream does not support writing"
    override _.Write(_, _, _) = notSupported "The stream does not support writing"
