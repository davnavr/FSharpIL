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
val compressedUnsigned : value: uint32 -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes a signed compressed integer (II.23.2).</summary>
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the <paramref name="value"/> is not in the range <c>-2^28</c> and <c>2^28 - 1</c> inclusive.
/// </exception>
val compressedSigned : value: int32 -> stream: byref<ChunkedMemoryBuilder> -> unit

/// Writes a type encoded in a signature (II.23.2.12).
val etype: EncodedType -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>MethodDef</c> (II.23.2.1).</summary>
val methodDefSig : signature: inref<MethodDefSig> -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a method reference used in the <c>MethodRef</c> table (II.23.2.1).</summary>
val methodRefSig: signature: inref<MethodRefSig> -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>Field</c> (II.23.2.4).</summary>
val fieldSig : signature: inref<FieldSig> -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the signature of a <c>Property</c> (II.23.2.5).</summary>
val propertySig : signature: inref<PropertySig> -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes a custom attribute (II.23.3).</summary>
val customAttrib : attrib: inref<CustomAttrib> -> stream: byref<ChunkedMemoryBuilder> -> unit

/// <summary>Writes the local variable types of a method (II.23.2.6).</summary>
/// <exception cref="T:System.ArgumentException">
/// Thrown when the number of local variables is greater than the maximum number of local variables allowed, or if no local
/// variables are declared.
/// </exception>
val localVarSig : signature: LocalVarSig -> stream: byref<ChunkedMemoryBuilder> -> unit // TODO: Make special collection type to enforce correct number of local variable types.
