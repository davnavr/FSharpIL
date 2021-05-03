namespace FSharpIL.Reading

open Microsoft.FSharp.Core.Operators.Checked

/// <summary>Represents an offset into the <c>#GUID</c> metadata heap (II.24.2.5).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedGuid (offset: uint32) =
    member _.Offset = offset
