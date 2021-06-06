/// Contains various magic numbers used throughout a PE file.
[<RequireQualifiedAccess>]
module FSharpIL.Magic // TODO: Move this to PortableExecutable namespace.

open System.Collections.Immutable

open FSharpIL.Utilities

let dosHeaderSignature = Convert.unsafeTo<_, ImmutableArray<byte>> "MZ"B

/// The PE signature, which immediately precedes the COFF header (II.25.2.1).
let portableExecutableSignature = Convert.unsafeTo<_, ImmutableArray<byte>> "PE\000\000"B

let coffHeaderSize = 20u

/// The size of the optional header in a PE32 file when all fields are present, in bytes (II.25.2.2).
let optionalHeaderSize = 0x224us

/// The size of a single section header, in bytes.
let sectionHeaderSize = 40u
