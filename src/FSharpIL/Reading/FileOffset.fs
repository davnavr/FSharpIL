namespace FSharpIL.Reading

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Utilities

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type FileOffset =
    internal { FileOffset: uint64 } // TODO: Make this a uint32 instead.
    static member op_Implicit { FileOffset = offset } = offset
    static member op_Implicit { FileOffset = Convert.U4 offset } = offset

    static member (+) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 + o2 }
    static member (+) ({ FileOffset = offset }, number) = { FileOffset = offset + number }
    static member inline (+) (number: uint64, offset: FileOffset) = offset + number
    static member inline (+) (offset: FileOffset, number: uint32) = offset + uint64 number
    static member inline (+) (number: uint32, offset: FileOffset) = uint64 number + offset

    static member (-) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 - o2 }
    static member (-) ({ FileOffset = offset }, number) = { FileOffset = offset - number }
    static member inline (-) (offset: FileOffset, number: uint32) = offset - uint64 number

    static member (*) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 * o2 }
    static member (*) ({ FileOffset = offset }, number) = { FileOffset = offset * number }
    static member inline (*) (offset: FileOffset, number: uint32) = offset * uint64 number
