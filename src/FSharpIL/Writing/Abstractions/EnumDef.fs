namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

// TODO: Figure out how to avoid addition of methods to Enum type. Maybe prevent conversion of EnumDef index to TypeDefRow index?
/// <summary>
/// Represents an enumeration type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Enum"/> (II.14.3).
/// </summary>
[<IsReadOnly>]
type EnumDef = struct
    val Access: TypeVisibility
    val EnumName: Identifier
    val Namespace: string
    val Flags: TypeDefFlags
    val Values: unit

    new (access: TypeVisibility, name, ns, serializable, values) =
        let mutable flags = access.Flags ||| TypeDefFlags.Sealed ||| TypeDefFlags.AutoLayout

        match serializable with
        | Serializable -> flags <- flags ||| TypeDefFlags.Serializable
        | NotSerializable -> ()

        { Access = access
          EnumName = name
          Namespace = ns
          Flags = flags
          Values = values }
end
