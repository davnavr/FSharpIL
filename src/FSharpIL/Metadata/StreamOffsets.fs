namespace FSharpIL.Metadata

open System.Runtime.CompilerServices

/// <summary>Represents an offset into the <c>#Strings</c> metadata stream (II.24.2.3).</summary>
[<IsReadOnly; Struct>]
type StringOffset =
    internal { StringOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.StringOffset
    static member op_Implicit { StringOffset = offset } = offset

/// <summary>Represents an index into the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
[<IsReadOnly; Struct>]
type GuidIndex =
    internal { GuidIndex: uint32 }
    member this.IsNull = this.GuidIndex = 0u
    override this.ToString() = sprintf "0x%X" this.GuidIndex
    static member op_Implicit { GuidIndex = offset } = offset
    static member Zero = { GuidIndex = 0u }

/// <summary>Represents an index into the <c>#US</c> metadata stream (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type UserStringOffset =
    internal { UserStringOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.UserStringOffset
    static member op_Implicit { UserStringOffset = offset } = offset

/// <summary>Represents an offset into the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type BlobOffset =
    internal { BlobOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.BlobOffset
    static member op_Implicit { BlobOffset = offset } = offset
