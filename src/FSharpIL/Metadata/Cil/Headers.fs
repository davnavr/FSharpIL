// TODO: Rename namespace to FSharpIL.Metadata.Bytecode or FSharpIL.Metadata.IntermediateLanguage or FSharpIL.IntermediateLanguage or FSharpIL.Metadata.InstructionSet
namespace FSharpIL.Metadata.Cil

open System

/// Specifies the type of the method header and additional information (II.25.4.4).
[<Flags>]
type ILMethodFlags =
    | TinyFormat = 0x2us
    | FatFormat = 0x3us
    | MoreSects = 0x8us
    | InitLocals = 0x10us

/// Describes a method body data section (II.25.4.5).
[<Flags>]
type ILSectionFlags =
    | None = 0uy
    | EHTable = 0x1uy
    | OptILTable = 0x2uy
    /// Indicates that the size of the method body data section takes up 3 bytes instead of 1 byte.
    | FatFormat = 0x40uy
    | MoreSects = 0x80uy
    | FatEHTable = 0x65uy

[<Flags>]
type ExceptionClauseFlags =
    | Exception = 0u
    | Filter = 1u
    | Finally = 2u
    | Fault = 4u
