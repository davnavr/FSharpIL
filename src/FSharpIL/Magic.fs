/// Contains various magic numbers and bytes used throughout a PE file containing CLI metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Magic

let portableExecutableSignature = [| 0x42uy; 0x53uy; 0x4Auy; 0x42uy; |]
