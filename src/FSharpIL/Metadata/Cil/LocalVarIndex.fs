namespace FSharpIL.Metadata.Cil

open FSharpIL.Utilities

[<System.Runtime.CompilerServices.IsReadOnly; Struct; RequireQualifiedAccess>]
type LocalVarIndex =
    private { Index: uint16 }
    override this.ToString() = string this.Index
    static member MinValue = { Index = 0us }
    static member MaxValue = { Index = FSharpIL.Metadata.Signatures.LocalVariable.maxLocalCount - 1us }
    static member op_Implicit { Index = i } = int32 i
    static member op_Implicit { Index = i } = uint32 i
    static member op_Implicit { Index = i } = int64 i
    static member op_Implicit { Index = i } = uint64 i
    static member op_Implicit { Index = i } = i
    static member op_Explicit(index: uint8) = { Index = uint16 index }
    static member op_Explicit(index: uint16) =
        let index' = { Index = index }
        if index' > LocalVarIndex.MaxValue then
            argOutOfRange (nameof index) index (sprintf "Local variable indices cannot exceed %O" LocalVarIndex.MaxValue)
        index'

[<AutoOpen>]
module LocalVarIndex =
    let inline (|LocalVarIndex|) (index: LocalVarIndex) = uint16 index

    /// <summary>Converts an integer into a local variable index.</summary>
    /// <exception cref="T:System.OverflowException">
    /// Thrown when an overflow occurs when the <paramref name="index"/> is converted into an unsigned 16-bit integer.
    /// </exception>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> exceeds the maximum valid local variable index.
    /// </exception>
    let inline locali index = LocalVarIndex.op_Explicit(Checked.uint16 index)
