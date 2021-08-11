// TODO: Turn this namespace into a module and rename it to FSharpIL.Metadata.InstructionSet or FSharpIL.InstructionSet
namespace FSharpIL.Metadata.Cil

open System
open System.Runtime.CompilerServices

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type InitLocals = | SkipInitLocals | InitLocals

/// Represents the maximum number of items "that can be pushed onto the CIL evaluation stack" for a method body (III.1.7.4).
[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type MaxStack =
    val private value: uint16
    new (value) = { value = value }
    override this.ToString() = sprintf ".maxstack %i" this.value
    static member Zero = MaxStack 0us
    static member MaxValue = MaxStack UInt16.MaxValue
    static member op_Implicit(value: MaxStack) = int64 value.value
    static member op_Implicit(value: MaxStack) = uint64 value.value
    static member op_Implicit(value: MaxStack) = int32 value.value
    static member op_Implicit(value: MaxStack) = uint32 value.value
    static member op_Implicit(value: MaxStack) = value.value
    /// <summary>Increments the <c>MaxStack</c> value by the given <paramref name="amount"/>.</summary>
    /// <exception cref="T:System.OverflowException">Thrown if an overflow occurs.</exception>
    static member (+) (value: MaxStack, amount: uint16) = MaxStack(Checked.(+) value.value amount)

/// Specifies the type of the method header and additional information (II.25.4.4).
[<Flags>]
type ILMethodFlags =
    | None = 0us
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
