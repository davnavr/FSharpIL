module FSharpIL.Reading.ParseBlob

open FSharpIL
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures.MetadataSignatures

val internal compressedUnsigned : chunk: byref<ChunkedMemory> -> Result<struct(byte * uint32), BlobError>

val fieldSig : chunk: inref<ChunkedMemory> -> Result<FieldSig, BlobError>

val methodDefSig : chunk: inref<ChunkedMemory> -> Result<MethodDefSig, BlobError>

//val methodRefSig : chunk: inref<ChunkedMemory> -> Result<MethodRefSig, BlobError>

val propertySig : chunk: inref<ChunkedMemory> -> Result<PropertySig, BlobError>

val typeSpec : chunk: inref<ChunkedMemory> -> Result<EncodedType, BlobError>

val customAttrib :
    fixedArgTypes: System.Collections.Immutable.ImmutableArray<ElemType> ->
    chunk: inref<ChunkedMemory> ->
    Result<CustomAttrib, BlobError>
