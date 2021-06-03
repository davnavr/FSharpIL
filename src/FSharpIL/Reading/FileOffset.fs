namespace FSharpIL.Reading

open Microsoft.FSharp.Core.Operators.Checked

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type FileOffset =
    internal { FileOffset: uint32 }
    static member op_Implicit { FileOffset = offset } = offset

    static member (+) ({ FileOffset = offset }, number) = { FileOffset = offset + number }
    static member inline (+) (number: uint32, offset: FileOffset) = offset + number
    static member (+) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 + o2 }

    static member (-) ({ FileOffset = offset }, number) = { FileOffset = offset - number }
    static member (-) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 - o2 }

    static member (*) ({ FileOffset = offset }, number) = { FileOffset = offset * number }
    static member inline (*) (number: uint32, offset: FileOffset) = offset * number
    static member (*) ({ FileOffset = o1 }, { FileOffset = o2 }) = { FileOffset = o1 * o2 }

    static member (/) ({ FileOffset = offset }, number) = { FileOffset = offset / number }
