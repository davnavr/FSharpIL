namespace FSharpIL.Metadata.Blobs

/// Describes how arguments are passed to a method (II.15.3).
[<System.Flags>]
type CallConvFlags =
    | HasThis = 0x20uy
    | ExplicitThis = 0x40uy
    | Default = 0uy
    | VarArg = 0x5uy
    | Generic = 0x10uy
