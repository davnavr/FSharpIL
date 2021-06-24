namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly>]
type MethodName = struct
    val Name: Identifier
    internal new (name) = { Name = name }
    override this.ToString() = this.Name.ToString()
end

[<RequireQualifiedAccess>]
module MethodName =
    let internal cctor = Identifier ".cctor"
    let internal ctor = Identifier ".ctor"

    let tryOfIdentifier name =
        if name <> ctor && name <> cctor
        then ValueSome(MethodName name)
        else ValueNone

    let tryOfStr str =
        match Identifier.tryOfStr str with
        | ValueSome name -> tryOfIdentifier name
        | ValueNone -> ValueNone

    let ofIdentifier name =
        match tryOfIdentifier name with
        | ValueSome mname -> mname
        | ValueNone -> invalidArg (nameof name) (sprintf "The method name \"%O\" is reserved for constructors" name)

    let ofStr str = ofIdentifier(Identifier.ofStr str)

    let toIdentifier (name: MethodName) = name.Name

[<AutoOpen>]
module MethodNamePatterns = let (|MethodName|) name = MethodName.toIdentifier name

[<Sealed>]
type Method internal (flags: MethodDefFlags, signature: MethodDefSig, name: Identifier) =
    member _.Flags = flags
    member _.Signature = signature
    member _.MethodName = name

    member _.Equals(other: Method) =
        if flags ||| other.Flags &&& MethodDefFlags.MemberAccessMask = MethodDefFlags.CompilerControlled
        then false
        else name = other.MethodName && signature = other.Signature

    override this.Equals obj =
        match obj with
        | :? Method as other -> this.Equals other
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(name, signature)

    interface IEquatable<Method> with member this.Equals other = this.Equals other

[<AutoOpen>]
module MethodHelpers =
    let private incGenParamCount (gcount: byref<uint32>) (ptype: inref<ParamItem>) =
        match ptype with
        | ParamItem.Param(_, t)
        | ParamItem.ByRef(_, t) when EncodedType.isMethodVar t -> gcount <- gcount + 1u
        |  _ -> ()

    let private getCallingConventions gcount =
        match gcount with
        | 0u -> Default
        | _ -> Generic gcount

    let methodRefSig hasThis returnType (parameterTypes: ImmutableArray<_>) =
        let mutable gcount = 0u
        for i = 0 to parameterTypes.Length do
            incGenParamCount &gcount (&parameterTypes.ItemRef i)

        { HasThis = hasThis
          CallingConvention = getCallingConventions gcount
          ReturnType = returnType
          Parameters = parameterTypes }

    let methodDefSig hasThis returnType (parameterTypes: ImmutableArray<_>) parameterList =
        let mutable parameters = Array.zeroCreate<Parameter> parameterTypes.Length
        let mutable gcount = 0u

        for i = 0 to parameterTypes.Length do
            let ptype = &parameterTypes.ItemRef i
            incGenParamCount &gcount &ptype
            parameters.[i] <-
                match parameterList i ptype with
                | ValueSome parameter -> parameter
                | ValueNone -> Unchecked.defaultof<_>

        struct (
            { HasThis = hasThis
              CallingConvention = getCallingConventions gcount
              ReturnType = returnType
              Parameters = parameterTypes },
            Unsafe.As<_, ImmutableArray<Parameter>> &parameters
        )

[<RequireQualifiedAccess>]
module MethodKinds =
    type IKind = interface
        inherit IAttributeTag<MethodDefFlags>
        abstract MethodThis: MethodThis
    end

    type Instance = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = HasThis
    end

    type Virtual = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Virtual
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = HasThis
    end

    type Final = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    HasThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Virtual ||| MethodDefFlags.Final
    end

    type Static = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Static
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = NoThis
    end

    type Abstract = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    HasThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Virtual ||| MethodDefFlags.Abstract
    end

[<IsReadOnly>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = struct
    val Definition: Method
    val Parameters: ImmutableArray<Parameter>

    new
        (
            visibility,
            flags: MethodAttributes<'Kind>,
            returnType,
            MethodName name,
            parameterTypes: ImmutableArray<_>,
            parameterList
        )
        =
        let struct(signature, parameters) =
            methodDefSig Unchecked.defaultof<'Kind>.MethodThis returnType parameterTypes parameterList
        { Definition = Method(flags.Flags ||| MemberVisibility.ofMethod visibility, signature, name)
          Parameters = parameters }
end

type InstanceMethodDef = MethodDefinition<MethodKinds.Instance>
type VirtualMethodDef = MethodDefinition<MethodKinds.Virtual>
type FinalMethodDef = MethodDefinition<MethodKinds.Final>
type StaticMethodDef = MethodDefinition<MethodKinds.Static>
type AbstractMethodDef = MethodDefinition<MethodKinds.Abstract>

[<IsReadOnly>]
type MethodReference<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = struct
    val Reference: Method

    new (visibility, flags: MethodAttributes<'Kind>, returnType, MethodName name, parameterTypes) =
        { Reference =
            Method (
                flags.Flags ||| ExternalVisibility.ofMethod visibility,
                methodRefSig Unchecked.defaultof<'Kind>.MethodThis returnType parameterTypes,
                name
            ) }
end

type InstanceMethodRef = MethodReference<MethodKinds.Instance>
type VirtualMethodRef = MethodReference<MethodKinds.Virtual>
type FinalMethodRef = MethodReference<MethodKinds.Final>
type StaticMethodRef = MethodReference<MethodKinds.Static>
type AbstractMethodRef = MethodReference<MethodKinds.Abstract>

[<IsReadOnly>]
type ConstructorDef = struct
    val Definition: Method
    val Parameters: ImmutableArray<Parameter>

    new (visibility, parameterTypes: ImmutableArray<ParamItem>, parameterList: ParameterList) =
        let struct(signature, parameters) = methodDefSig HasThis ReturnType.unmodifiedVoid parameterTypes parameterList
        { Definition =
            Method (
                MemberVisibility.ofMethod visibility ||| MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName,
                signature,
                MethodName.ctor
            )
          Parameters = parameters }
end

[<IsReadOnly>]
type ConstructorRef = struct
    val Reference: Method

    new (visibility, parameterTypes) =
        { Reference =
            Method (
                ExternalVisibility.ofMethod visibility ||| MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName,
                methodRefSig HasThis ReturnType.unmodifiedVoid parameterTypes,
                MethodName.ctor
            ) }
end

[<RequireQualifiedAccess>]
type DefinedMethod =
    | Instance of InstanceMethodDef
    | Virtual of VirtualMethodDef
    | Final of FinalMethodDef
    | Static of StaticMethodDef
    | ObjectConstructor of ConstructorDef
    | ClassConstructor // of ?
    | Abstract of AbstractMethodDef
    //| PInvokeMethod

[<RequireQualifiedAccess>]
type ReferencedMethod =
    | Instance of InstanceMethodRef
    | Virtual of VirtualMethodRef
    | Final of FinalMethodRef
    | Static of StaticMethodRef
    | ObjectConstructor of ConstructorRef
    | Abstract of AbstractMethodRef
