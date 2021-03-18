namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Represents an element type used in a signature (II.23.1.16).
type internal ElementType =
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

    | ValueType = 0x11uy
    | Class = 0x12uy
    | Var = 0x13uy
    | Array = 0x14uy
    | GenericInst = 0x15uy

    | SZArray = 0x1Duy
    | MVar = 0x1Euy

    | Sentinel = 0x41uy

type internal ITypeDefOrRefOrSpec = interface end

/// (II.23.2.7)
[<IsReadOnly; Struct>]
type CustomModifier = struct
    val Required: bool
    val internal CMod: ITypeDefOrRefOrSpec // ModifierType

    internal new (required, modifierType) = { Required = required; CMod = modifierType }
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
