﻿namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

[<Interface>] type internal ITypeDefOrRefOrSpec = inherit IIndexValue

/// (II.23.2.7)
[<IsReadOnly; Struct>]
type CustomModifier =
    struct
        val Required: bool
        val internal CMod: ITypeDefOrRefOrSpec // ModifierType

        internal new (required, modifierType) = { Required = required; CMod = modifierType }

        member internal this.CheckOwner owner = this.CMod.CheckOwner owner

        interface IIndexValue with member this.CheckOwner owner = this.CheckOwner owner
    end

[<Interface>] type internal IReturnType = inherit IIndexValue

/// <summary>Represents a <c>RetType</c> used in a signature (II.23.2.11).</summary>
[<IsReadOnly>]
type ReturnTypeItem =
    struct
        val CustomMod: ImmutableArray<CustomModifier>
        val internal RetType: IReturnType // ReturnType

        internal new (modifiers, returnType) = { CustomMod = modifiers; RetType = returnType }

        member internal this.CheckOwner owner =
            for cmod in this.CustomMod do cmod.CheckOwner owner
            this.RetType.CheckOwner owner

        interface IIndexValue with member this.CheckOwner owner = this.CheckOwner owner
    end

[<Interface>] type internal IEncodedType = inherit IIndexValue
