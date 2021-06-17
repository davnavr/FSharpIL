[<RequireQualifiedAccess>]
module FSharpIL.Writing.BlobWriter

open FSharpIL
open FSharpIL.Metadata.Signatures

[<Literal>] val MaxCompressedUnsigned : uint32 = 0x1FFF_FFFFu
[<Literal>] val MaxCompressedSigned : int32 = 0x0FFFFFFF
[<Literal>] val MinCompressedSigned : int32 = 0xF0000000

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

/// <summary>Writes a signed compressed integer (II.23.2).</summary>
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the <paramref name="value"/> is not in the range <c>-2^28</c> and <c>2^28 - 1</c> inclusive.
/// </exception>
val compressedSigned : value: int32 -> byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>MethodDef</c> (II.23.2.1).</summary>
val methodDefSig : signature: inref<MethodDefSig> -> byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>Field</c> (II.23.2.4).</summary>
val fieldSig : signature: inref<FieldSig> -> byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>Property</c> (II.23.2.5).</summary>
val propertySig : signature: inref<PropertySig> -> byref<ChunkedMemoryBuilder> -> unit
