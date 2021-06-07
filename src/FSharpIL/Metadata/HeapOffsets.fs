namespace FSharpIL.Metadata

open System.Runtime.CompilerServices

/// <summary>Represents an offset into the <c>#Strings</c> metadata heap (II.24.2.3).</summary>
[<IsReadOnly; Struct>]
type StringOffset =
    internal { StringOffset: uint32 }
    static member op_Implicit { StringOffset = offset } = offset

/// <summary>Represents an index into the <c>#GUID</c> metadata heap (II.24.2.5).</summary>
[<IsReadOnly; Struct>]
type GuidIndex =
    internal { GuidIndex: uint32 }
    static member op_Implicit { GuidIndex = offset } = offset

/// <summary>Represents an index into the <c>#US</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type UserStringOffset =
    internal { UserStringOffset: uint32 }
    static member op_Implicit { UserStringOffset = offset } = offset

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type BlobOffset =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset
