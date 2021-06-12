namespace FSharpIL

open System.Runtime.CompilerServices

/// Represents a relative virtual address.
[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type Rva = struct // TODO: Move Rva types to PortableExecutable namespace and create new type for MethodBodyRva.
    val private value: uint32
    new (value) = { value = value }
    static member inline Zero = Rva 0u
    static member op_Implicit(rva: Rva) = uint64 rva.value
    static member op_Implicit(rva: Rva) = rva.value
    static member (+) (rva: Rva, value: uint32) = Rva(rva.value + value)
    static member inline (+) (value: uint32, rva: Rva) = rva + value
    static member (+) (left: Rva, right: Rva) = Rva(left.value + right.value)
    static member (*) (rva: Rva, value: uint32) = Rva(rva.value * value)
    static member (/) (rva: Rva, value: uint32) = Rva(rva.value / value)
    static member (-) (rva: Rva, value: uint32) = Rva(rva.value - value)
    static member (-) (left: Rva, right: Rva) = Rva(left.value - right.value)
    override this.ToString() = sprintf "0x%08X" this.value
end

[<IsReadOnly; Struct>]
type RvaAndSize =
    { Rva: Rva; Size: uint32 }
    static member inline Zero = { Rva = Rva.Zero; Size = 0u }
