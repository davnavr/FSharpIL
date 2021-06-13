namespace FSharpIL.Writing.Tables.Emit

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Tables

[<IsReadOnly>]
type DelegateParam = struct
    val Flags: ParamFlags
    val Name: string
    val Type: ParamItem
    internal new (flags, name, signature) = { Flags = flags; Name = name; Type = signature }
end

[<RequireQualifiedAccess>]
module DelegateParam =
    let Param (name, signature) = DelegateParam(ParamFlags.None, name, signature)
    let InRef (name, signature) = DelegateParam(ParamFlags.In, name, signature)
    let OutRef (name, signature) = DelegateParam(ParamFlags.Out, name, signature)
    let Ref (name, signature) = DelegateParam(ParamFlags.Out ||| ParamFlags.In, name, signature)

/// <summary>
/// Represents a delegate type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Delegate"/> (I.8.9.3 and II.14.6).
/// </summary>
[<IsReadOnly>]
type DelegateDef = struct
    val Access: TypeVisibility
    val ReturnType: ReturnType
    val Parameters: ImmutableArray<DelegateParam>
    val Name: Identifier
    val Namespace: string
    val Flags: TypeDefFlags
    /// <summary>
    /// Attributes applied to the <c>MethodDef</c> row corresponding to the <c>Invoke</c> method of the delegate type.
    /// </summary>
    val InvokeFlags: MethodDefFlags

    internal new (access, returnType, parameters, name, ns, tflags, mflags) =
        { Access = access
          ReturnType = returnType
          Parameters = parameters
          Name = name
          Namespace = ns
          Flags = tflags
          InvokeFlags = mflags }

    interface ITableRow
end

[<RequireQualifiedAccess>]
module Delegate =
    let typeIndex ({ TableIndex = index }: TableIndex<DelegateDef>): TableIndex<TypeDefRow> = { TableIndex = index }

    let create (access: TypeVisibility) returnType parameters delegateName delegateNamespace serializable mlayout mstrict =
        DelegateDef (
            access,
            returnType,
            parameters,
            delegateName,
            delegateNamespace,
            TypeDefFlags.AutoLayout |||
            TypeDefFlags.AnsiClass |||
            TypeDefFlags.Sealed |||
            Flags.serializable serializable |||
            access.Flags,
            MethodDefFlags.Public |||
            MethodDefFlags.HideBySig |||
            MethodDefFlags.Virtual |||
            Flags.vTableLayout mlayout |||
            Flags.strict mstrict
        )

    let tryAddDef (def: inref<DelegateDef>) (builder: TypeDefTableBuilder) =
        failwith "TODO: Add delegate"
