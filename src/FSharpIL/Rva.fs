namespace FSharpIL

/// Represents a relative virtual address.
[<System.Runtime.CompilerServices.IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type Rva = struct
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
end
