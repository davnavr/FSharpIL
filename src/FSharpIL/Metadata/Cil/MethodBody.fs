// TODO: Rename namespace to FSharpIL.Bytecode or FSharpIL.IntermediateLanguage or FSharpIL.InstructionSet
namespace FSharpIL.Metadata.Cil

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type InitLocals =
    | InitLocals
    | SkipInitLocals

[<Sealed>]
type MethodBody =
    val InstructionStream: FSharpIL.ChunkedMemory
    val MaxStack: uint16
    val LocalVarSigTok: TableIndex<StandaloneSigRow>
    //val DataSections
    val InitLocals: InitLocals

    // TODO: In constructor, set InitLocals false if no local variables are present.
    internal new (instructions, maxStack, localVarSigTok, initLocals) =
        { InstructionStream = instructions
          MaxStack = maxStack
          LocalVarSigTok = localVarSigTok
          InitLocals = if localVarSigTok.IsNull then SkipInitLocals else initLocals }

    member this.CodeSize = this.InstructionStream.Length

/// Specifies the type of the method header and additional information (II.25.4.4).
[<Flags>]
type ILMethodFlags =
    | None = 0us
    | TinyFormat = 0x2us
    | FatFormat = 0x3us
    | MoreSects = 0x8us
    | InitLocals = 0x10us

[<RequireQualifiedAccess>]
module internal MethodBody =
    let [<Literal>] MaxTinyBodySize = 63u
    let [<Literal>] MaxTinyMaxStack = 8us
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    let [<Literal>] FatFormatSize = 3u

type MethodBody with
    member this.Flags =
        let mutable flags = ILMethodFlags.None

        //if exceptions present
        //if extra data sections present

        if
            flags = ILMethodFlags.None
            && this.CodeSize <= MethodBody.MaxTinyBodySize
            && this.LocalVarSigTok.IsNull
            && this.MaxStack <= MethodBody.MaxTinyMaxStack
        then
                flags <- ILMethodFlags.TinyFormat
            else
                flags <- flags ||| ILMethodFlags.FatFormat

        flags

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
