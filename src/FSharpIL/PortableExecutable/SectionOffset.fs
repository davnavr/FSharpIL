namespace FSharpIL.PortableExecutable

/// An offset from the start of a section.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type SectionOffset =
    internal { SectionOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.SectionOffset
    static member op_Implicit { SectionOffset = offset } = offset
    static member (+) ({ SectionOffset = left }, { SectionOffset = right }) = { FileOffset = left + right }
    static member (-) ({ SectionOffset = left }, { SectionOffset = right }) = { FileOffset = left - right }
    static member (*) ({ SectionOffset = offset }, multiplier) = { FileOffset = offset * multiplier }
    static member inline (*) (multiplier: uint32, offset: SectionOffset) = offset * multiplier
