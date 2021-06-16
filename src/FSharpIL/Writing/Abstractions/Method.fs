namespace FSharpIL.Writing.Abstractions

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables
open FSharpIL.Writing

//[<IsReadOnly>]
//type PInvokeMethodDef = struct
//    val Visibility: MemberVisibility
//    val ReturnType: ReturnType
//    //val CallingConvention: UnmanagedCallingConvention
//    val Name: Identifier
//    val Parameters: ImmutableArray<Parameter>

//    interface ITableRow
//end

[<RequireQualifiedAccess>]
module MethodFlagTags =
    type Tag internal () = class end
    type Default private () = inherit Tag()
    type Virtual private () = inherit Tag()

[<IsReadOnly>]
type ManagedMethodFlags<'Tag when 'Tag :> MethodFlagTags.Tag> internal (flags: MethodDefFlags) = struct
    static member val HideBySig = ManagedMethodFlags<'Tag> MethodDefFlags.HideBySig
    member _.Flags = flags
    static member (|||) (left: ManagedMethodFlags<'Tag>, right: ManagedMethodFlags<'Tag>) =
        ManagedMethodFlags<'Tag>(left.Flags ||| right.Flags)
    static member op_Implicit(flags: ManagedMethodFlags<'Tag>) = uint32 flags.Flags
    override this.ToString() = this.Flags.ToString()
end

[<RequireQualifiedAccess>]
module ManagedMethodKinds =
    type IKind<'Body when 'Body : struct> = interface
        abstract MethodBody: 'Body -> MethodBodyLocation
        abstract MethodThis: MethodThis
        abstract RequiredFlags: MethodDefFlags
    end

    [<Struct>]
    type Concrete =
        interface IKind<MethodBodyLocation> with
            member _.MethodBody body = body
            member _.MethodThis = HasThis
            member _.RequiredFlags = Unchecked.defaultof<_>

    [<Struct>]
    type Final =
        interface IKind<MethodBodyLocation> with
            member _.MethodBody body = body
            member _.MethodThis = HasThis
            member _.RequiredFlags = MethodDefFlags.Virtual ||| MethodDefFlags.Final

    [<Struct>]
    type Virtual =
        interface IKind<MethodBodyLocation> with
            member _.MethodBody body = body
            member _.MethodThis = HasThis
            member _.RequiredFlags = MethodDefFlags.Virtual

    [<Struct>]
    type Abstract =
        interface IKind<Omitted> with
            member _.MethodBody _ = MethodBodyLocation 0u
            member _.MethodThis = HasThis
            member _.RequiredFlags = MethodDefFlags.Virtual ||| MethodDefFlags.Abstract

    [<Struct>]
    type Static =
        interface IKind<MethodBodyLocation> with
            member _.MethodBody body = body
            member _.MethodThis = NoThis
            member _.RequiredFlags = MethodDefFlags.Static


type ManagedMethodDef<'Kind, 'Flag, 'Body
    when 'Body : struct
    and 'Flag :> MethodFlagTags.Tag
    and 'Kind :> ManagedMethodKinds.IKind<'Body>
    and 'Kind : struct> =
    { Visibility: MemberVisibility
      Flags: ManagedMethodFlags<'Flag>
      ReturnType: ReturnType
      MethodName: Identifier // TODO: Have special method name type to prevent .ctor or .cctor from being used.
      Parameters: ImmutableArray<Parameter>
      Body: 'Body }

    interface ITableRow

type DefaultMethodFlags = ManagedMethodFlags<MethodFlagTags.Default>
type VirtualMethodFlags = ManagedMethodFlags<MethodFlagTags.Virtual>

[<RequireQualifiedAccess>]
module VirtualMethodFlags =
    let NewSlot = VirtualMethodFlags MethodDefFlags.NewSlot
    let Strict = VirtualMethodFlags MethodDefFlags.Strict

type ConcreteMethodDef = ManagedMethodDef<ManagedMethodKinds.Concrete, MethodFlagTags.Default, MethodBodyLocation>
type FinalMethodDef = ManagedMethodDef<ManagedMethodKinds.Final, MethodFlagTags.Default, MethodBodyLocation>
type VirtualMethodDef = ManagedMethodDef<ManagedMethodKinds.Virtual, MethodFlagTags.Default, MethodBodyLocation>
type AbstractMethodDef = ManagedMethodDef<ManagedMethodKinds.Abstract, MethodFlagTags.Default, Omitted>
type StaticMethodDef = ManagedMethodDef<ManagedMethodKinds.Static, MethodFlagTags.Default, MethodBodyLocation>

[<RequireQualifiedAccess>]
module Method =
    let rec private isGenericParam t =
        match t with
        | EncodedType.MVar _ -> true
        | EncodedType.Array(item, _)
        | EncodedType.SZArray(_, item)
        | EncodedType.Ptr(Pointer.Type(_, item)) -> isGenericParam item
        // TODO: Don't forget to check if function pointer types also use MVar.
        | _ -> false

    let private parameters (method: inref<ManagedMethodDef<_, _, _>>) strings =
        match method.Parameters.Length with
        | 0 -> struct(Default, ImmutableArray<ParamItem>.Empty)
        | length ->
            let mutable items, gcount = Array.zeroCreate<ParamItem> length, 0u

            for i = 0 to length - 1 do
                let param = &method.Parameters.ItemRef i
                let item = Parameter.item &param

                Parameter.row (Checked.uint16 i) &param strings |> failwith "TODO: Add parameter rows"

                if isGenericParam param.Type then gcount <- gcount + 1u

                items.[i] <- item

            let cconv =
                match gcount with
                | 0u ->  Default
                | _ -> Generic gcount

            struct(cconv, Unsafe.As &items)

    let private tryAddRow owner (method: inref<ManagedMethodDef<'Kind, _, _>>) strings (blobs: BlobStreamBuilder) members =
        let mutable entry = TypeMemberMap.findMembers owner members
        let struct(cconv, paramSigItems) = parameters &method strings

        let method' =
            { Rva = Unchecked.defaultof<'Kind>.MethodBody method.Body
              ImplFlags = MethodImplFlags.IL
              Flags =
                MemberVisibility.ofMethod method.Visibility
                ||| method.Flags.Flags
                ||| Unchecked.defaultof<'Kind>.RequiredFlags
              Name = strings.Add method.MethodName
              Signature =
                { CallingConvention = cconv
                  HasThis = Unchecked.defaultof<'Kind>.MethodThis
                  ReturnType = method.ReturnType
                  Parameters = paramSigItems }
                |> failwith "TODO: Add Signature to blobs stream"
              ParamList = failwith "TODO: Get params" }

        entry.Methods.Add &method' |> ignore
        members.MemberMap.[owner] <- entry
        failwith "TODO: Should duplicate checking happen when entry is modified, or when type member map is serialized?"

    let tryAddConcrete (MemberOwner owner: InstanceMemberOwner) (method: inref<ConcreteMethodDef>) strings blobs members =
        tryAddRow owner &method strings blobs members

    //let tryAddFinal
    //let tryAddVirtual

    let tryAddAbstract (MemberOwner owner: AbstractMemberOwner) (method: inref<AbstractMethodDef>) strings blobs members =
        tryAddRow owner &method strings blobs members

    let tryAddStatic (MemberOwner owner: StaticMemberOwner) (method: inref<StaticMethodDef>) strings blobs members =
        tryAddRow owner &method strings blobs members
