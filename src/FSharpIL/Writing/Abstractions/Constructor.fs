namespace FSharpIL.Writing.Abstractions

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<RequireQualifiedAccess>]
module ConstructorKinds =
    type Kind = abstract Name: Identifier
    type [<Struct>] Object = interface Kind with member _.Name = Identifier.ofStr ".ctor"
    type [<Struct>] Class = interface Kind with member _.Name = Identifier.ofStr ".cctor"

[<System.Runtime.CompilerServices.IsReadOnly>]
type ConstructorDef<'Kind when 'Kind : struct and 'Kind :> ConstructorKinds.Kind> = struct
    val Visibility: MemberVisibility
    val ReturnType: ReturnType
    val Parameters: ImmutableArray<Parameter>
    val Body: MethodBodyLocation

    new (visibility, retType, paramList, body) =
        { Visibility = visibility
          ReturnType = retType
          Parameters = paramList
          Body = body }

    member _.MethodName = Unchecked.defaultof<'Kind>.Name

    interface ITableRow
end

type ObjectConstructorDef = ConstructorDef<ConstructorKinds.Object>
type ClassConstructorDef = ConstructorDef<ConstructorKinds.Class>
