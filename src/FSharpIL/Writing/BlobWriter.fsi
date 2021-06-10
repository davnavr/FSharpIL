[<RequireQualifiedAccess>]
module FSharpIL.Writing.BlobWriter

open FSharpIL

[<Literal>] val MaxCompressedUnsigned : uint32 = 0x1FFF_FFFFu

/// Calculates how many bytes would be taken up if the specified integer was compressed (II.23.2).
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the <paramref name="value"/> cannot be compressed.
/// </exception>
val compressedUnsignedSize : value: uint32 -> uint32

/// <summary>Writes a compressed unsigned integer (II.23.2).</summary>
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the <paramref name="value"/> is greater than the maximum compressed unsigned integer.
/// </exception>
val compressedUnsigned : value: uint32 -> byref<ChunkedMemoryBuilder> -> unit
