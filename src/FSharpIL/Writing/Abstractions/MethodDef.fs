namespace FSharpIL.Writing.Abstractions

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly>]
type PInvokeMethodDef = struct
    val Visibility: MemberVisibility
    val ReturnType: ReturnType
    //val CallingConvention: UnmanagedCallingConvention
    val Name: Identifier
    val Parameters: ImmutableArray<ParamItem>

    interface ITableRow
end

[<RequireQualifiedAccess>]
module ManagedMethodKinds =
    type Concrete = struct end
    type Final = struct end
    type Abstract = struct end
    type Static = struct end

[<IsReadOnly; Struct>]
type ManagedMethodDef<'Kind, 'Body when 'Body : struct> =
    { Visibility: MemberVisibility
      ReturnType: ReturnType
      MethodName: Identifier
      Parameters: ImmutableArray<ParamItem>
      Body: MethodBodyLocation }

    interface ITableRow

type ConcreteMethodDef = ManagedMethodDef<ManagedMethodKinds.Concrete, MethodBodyLocation>
type FinalMethodDef = ManagedMethodDef<ManagedMethodKinds.Final, MethodBodyLocation>
type AbstractMethodDef = ManagedMethodDef<ManagedMethodKinds.Abstract, Omitted>
type StaticMethodDef = ManagedMethodDef<ManagedMethodKinds.Static, MethodBodyLocation>

[<RequireQualifiedAccess>]
module ConstructorKinds =
    type Kind = abstract Name: Identifier
    type [<Struct>] Object = interface Kind with member _.Name = Identifier.ofStr ".ctor"
    type [<Struct>] Class = interface Kind with member _.Name = Identifier.ofStr ".cctor"

[<IsReadOnly>]
type ConstructorDef<'Kind when 'Kind : struct and 'Kind :> ConstructorKinds.Kind> = struct
    val Visibility: MemberVisibility
    val ReturnType: ReturnType
    val Parameters: ImmutableArray<ParamItem>
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
