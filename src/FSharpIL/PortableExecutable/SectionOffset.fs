namespace FSharpIL.PortableExecutable

open FSharpIL

/// An offset from the start of a section.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type SectionOffset =
    internal { SectionOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.SectionOffset
    static member op_Implicit { SectionOffset = offset } = offset
    static member (+) (rva: Rva, { SectionOffset = offset }) = Rva(uint32 rva + offset)
    static member (+) ({ SectionOffset = soffset }, offset: uint32) = { SectionOffset = soffset + offset }
    static member (+) ({ SectionOffset = left }, { SectionOffset = right }) = { SectionOffset = left + right }
    static member (-) ({ SectionOffset = left }, { SectionOffset = right }) = { SectionOffset = left - right }
    static member (*) ({ SectionOffset = offset }, multiplier) = { SectionOffset = offset * multiplier }
    static member inline (*) (multiplier: uint32, offset: SectionOffset) = offset * multiplier
