namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Represents an element type used in a signature (II.23.1.16).
type ElementType =
    | End = 0uy
    | Void = 0x1uy
    | Boolean = 0x2uy
    | Char = 0x3uy
    | I1 = 0x4uy
    | U1 = 0x5uy
    | I2 = 0x6uy
    | U2 = 0x7uy
    | I4 = 0x8uy
    | U4 = 0x9uy
    | I8 = 0xAuy
    | U8 = 0xBuy
    | R4 = 0xCuy
    | R8 = 0xDuy
    | String = 0xEuy
    | Ptr = 0xFuy
    | ByRef = 0x10uy
    | ValueType = 0x11uy
    | Class = 0x12uy
    | Var = 0x13uy
    | Array = 0x14uy
    | GenericInst = 0x15uy
    | TypedByRef = 0x16uy
    | I = 0x18uy
    | U = 0x19uy
    | FnPtr = 0x1Buy
    | Object = 0x1Cuy
    | SZArray = 0x1Duy
    | MVar = 0x1Euy
    | CModReqd = 0x1Fuy
    | CModOpt = 0x20uy
    | Internal = 0x21uy
    | Modifier = 0x40uy
    | Sentinel = 0x41uy
    | Pinned = 0x45uy
    | Type = 50uy
    | Boxed = 0x51uy
    | Field = 0x53uy
    | Property = 0x54uy
    | Enum = 0x55uy

type TypeDefOrRefOrSpecTag =
    | Def = 0uy
    | Ref = 1uy
    | Spec = 2uy

/// <summary>
/// Represents a token or index corresponding to a row in the <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c> table (II.23.2.8).
/// </summary>
type TypeDefOrRefOrSpecEncoded = TaggedIndex<TypeDefOrRefOrSpecTag>

/// (II.23.2.7)
[<IsReadOnly; Struct>]
type CustomModifier = struct
    val Required: bool
    val ModifierType: TypeDefOrRefOrSpecEncoded
    new (required, modifierType) = { Required = required; ModifierType = modifierType }
end

type internal IReturnType = interface end

[<Sealed>]
type internal ReturnTypeVoid private () =
    static member Item = ReturnTypeVoid()
    interface IReturnType

/// <summary>Represents a <c>RetType</c> used in a signature (II.23.2.11).</summary>
[<IsReadOnly>]
type ReturnTypeItem = struct
    val CustomMod: ImmutableArray<CustomModifier>
    val internal RetType: IReturnType // ReturnType

    internal new (modifiers, returnType) = { CustomMod = modifiers; RetType = returnType }
    internal new (returnType) = ReturnTypeItem(ImmutableArray.Empty, returnType)

    override this.ToString() = this.RetType.ToString()
end

type internal IEncodedType = interface end
