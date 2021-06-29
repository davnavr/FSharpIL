namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Cli.Signatures

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

[<AbstractClass>]
type Method =
    val Flags: MethodDefFlags
    val HasThis: MethodThis
    val CallingConvention: CallingConventions
    val Name: Identifier
    val ReturnType: ReturnType
    val ParameterTypes: ImmutableArray<ParamItem>

    new (flags, mthis, cconv, name, rtype, ptypes) =
        { Flags = flags
          HasThis = mthis
          CallingConvention = cconv
          Name = name
          ReturnType = rtype
          ParameterTypes = ptypes }

    member this.Signature =
        { HasThis = this.HasThis
          CallingConvention = this.CallingConvention
          ReturnType = this.ReturnType
          Parameters = this.ParameterTypes }

    member this.Equals(other: #Method) =
        if this.Flags ||| other.Flags &&& MethodDefFlags.MemberAccessMask = MethodDefFlags.CompilerControlled
        then false
        else
            this.Name = other.Name
            && this.ParameterTypes = other.ParameterTypes
            && this.HasThis = other.HasThis
            && this.ReturnType = other.ReturnType
            && this.CallingConvention = other.CallingConvention

    override this.Equals obj =
        match obj with
        | :? Method as other -> this.Equals(other = other)
        | _ -> false

    override this.GetHashCode() =
        HashCode.Combine(this.HasThis, this.CallingConvention, this.Name, this.ReturnType, this.ParameterTypes)

    interface IEquatable<Method> with member this.Equals other = this.Equals other

[<AutoOpen>]
module MethodHelpers =
    type IParameterIterator<'State> = interface
        abstract Update: int32 * 'State * inref<ParamItem> -> unit
    end

    type EmptyParameterIterator = struct
        interface IParameterIterator<Omitted> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member _.Update(_, _, _) = ()
    end

    type MethodDefParamIterator = struct
        interface IParameterIterator<struct(Parameter[] * ParameterList)> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member _.Update(i, (parameters, generator), ptype) = parameters.[i] <- generator i ptype
    end

    let checkMethodSig<'Iter, 'State when 'Iter :> IParameterIterator<'State> and 'Iter : struct>
        (state: 'State)
        (parameterTypes: ImmutableArray<ParamItem>)
        =
        let mutable gcount = 0u
        for i = 0 to parameterTypes.Length - 1 do
            let ptype = &parameterTypes.ItemRef i

            match ptype with
            | ParamItem.Param(_, t)
            | ParamItem.ByRef(_, t) when EncodedType.isMethodVar t -> gcount <- gcount + 1u
            |  _ -> ()

            Unchecked.defaultof<'Iter>.Update(i, state, &ptype)

        if gcount > 0u
        then CallingConventions.Generic gcount
        else CallingConventions.Default

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
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = HasThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = MethodDefFlags.Virtual ||| MethodDefFlags.Final
    end

    type Static = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Static
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = NoThis
    end

    type Abstract = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = HasThis

            member _.RequiredFlags
                with[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = MethodDefFlags.Virtual ||| MethodDefFlags.Abstract
    end

    type ObjectConstructor = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = HasThis

            member _.RequiredFlags
                with[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName
    end

    type ClassConstructor = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = NoThis

            member _.RequiredFlags
                with[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = MethodDefFlags.Static ||| MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName
    end

[<AbstractClass>]
type DefinedMethod =
    inherit Method

    val ImplFlags: MethodImplFlags
    val Parameters: ImmutableArray<Parameter>

    new (iflags, flags, mthis, rtype, MethodName name, parameterTypes: ImmutableArray<_>, parameterList) =
        let mutable parameters = Array.zeroCreate parameterTypes.Length
        let cconv = checkMethodSig<MethodDefParamIterator, _> (parameters, parameterList) parameterTypes
        { inherit Method (
              flags,
              mthis,
              cconv,
              name,
              rtype,
              parameterTypes
          )
          ImplFlags = iflags
          Parameters = Unsafe.As &parameters }

[<Sealed>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct>
    (
        visibility,
        flags: MethodAttributes<'Kind>,
        name,
        rtype,
        parameterTypes,
        parameterList
    )
    =
    inherit DefinedMethod (
        MethodImplFlags.IL, // TODO: Set PInvokeImpl flag for PInvoke methods.
        Unchecked.defaultof<'Kind>.RequiredFlags ||| flags.Flags ||| MemberVisibility.ofMethod visibility,
        Unchecked.defaultof<'Kind>.MethodThis,
        name,
        rtype,
        parameterTypes,
        parameterList
    )

[<IsReadOnly; Struct>]
type EntryPointKind =
    { ReturnExitCode: bool
      ArgumentsName: Identifier voption option }

    member this.HasArguments = Option.isSome this.ArgumentsName

[<RequireQualifiedAccess>]
module EntryPointKind =
    let exitCodeType = ReturnType.Type(ImmutableArray.Empty, EncodedType.I4)

    let private argsParameterTypes =
        ImmutableArray.Create<ParamItem>(
            ParamItem.Param(ImmutableArray.Empty, EncodedType.SZArray(ImmutableArray.Empty, EncodedType.String))
        )

    let ExitWithArgs argsParamName = { ReturnExitCode = true; ArgumentsName = Some argsParamName }
    let VoidWithArgs argsParamName = { ReturnExitCode = false; ArgumentsName = Some argsParamName }
    let ExitNoArgs = { ReturnExitCode = true; ArgumentsName = None }
    let VoidNoArgs = { ReturnExitCode = false; ArgumentsName = None }

    let inline returnType kind = if kind.ReturnExitCode then exitCodeType else ReturnType.RVoid

    let parameterTypes kind =
        match kind with
        | { ArgumentsName = Some _ } -> argsParameterTypes
        | { ArgumentsName = None } -> ImmutableArray.Empty

    let parameterList kind: ParameterList =
        match kind with
        | { ArgumentsName = Some name } ->
            fun _ _ ->
                { Kind = ParameterKind.Default
                  DefaultValue = ValueNone
                  ParamName = name }
        | { ArgumentsName = None } -> Parameter.emptyList

[<IsReadOnly>]
type EntryPointMethod = struct
    val Method: MethodDefinition<MethodKinds.Static>

    new (method) = { Method = method }

    member this.Kind =
        { ReturnExitCode = this.Method.ReturnType = EntryPointKind.exitCodeType
          ArgumentsName = if this.Method.Parameters.Length > 0 then Some(this.Method.Parameters.ItemRef(0).ParamName) else None }
end

type DefinedMethod with
    static member Instance(visibility, flags, returnType, name, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.Instance>(visibility, flags, returnType, name, parameterTypes, parameterList)

    static member Abstract(visibility, flags, returnType, name, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.Abstract>(visibility, flags, returnType, name, parameterTypes, parameterList)

    static member Final(visibility, flags, returnType, name, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.Final>(visibility, flags, returnType, name, parameterTypes, parameterList)

    static member Static(visibility, flags, returnType, name, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.Static>(visibility, flags, returnType, name, parameterTypes, parameterList)

    static member Virtual(visibility, flags, returnType, name, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.Virtual>(visibility, flags, returnType, name, parameterTypes, parameterList)

    static member Constructor(visibility, flags, parameterTypes, parameterList) =
        new MethodDefinition<MethodKinds.ObjectConstructor> (
            visibility,
            flags,
            ReturnType.RVoid,
            MethodName MethodName.ctor,
            parameterTypes,
            parameterList
        )

    static member EntryPoint(visibility, flags, name, kind) =
        DefinedMethod.Static (
            visibility,
            flags,
            EntryPointKind.returnType kind,
            name,
            EntryPointKind.parameterTypes kind,
            EntryPointKind.parameterList kind
        )
        |> EntryPointMethod

[<AutoOpen>]
module ConstructorHelpers =
    let classConstructorDef =
        new MethodDefinition<MethodKinds.ClassConstructor> (
            MemberVisibility.Private,
            MethodAttributes(),
            ReturnType.RVoid,
            MethodName MethodName.cctor,
            ImmutableArray.Empty,
            Unchecked.defaultof<_>
        )

type DefinedMethod with static member ClassConstructor = classConstructorDef

[<AbstractClass>]
type ReferencedMethod =
    inherit Method

    new (flags, mthis, rtype, MethodName name, parameterTypes: ImmutableArray<_>) =
        let cconv = checkMethodSig<EmptyParameterIterator, _> Omitted parameterTypes
        { inherit Method (
            flags,
            mthis,
            cconv,
            name,
            rtype,
            parameterTypes
          ) }

[<Sealed>]
type MethodReference<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct>
    (
        visibility,
        flags: MethodAttributes<'Kind>,
        returnType,
        name,
        parameterTypes
    )
    =
    inherit ReferencedMethod (
        flags.Flags ||| ExternalVisibility.ofMethod visibility,
        Unchecked.defaultof<'Kind>.MethodThis,
        returnType,
        name,
        parameterTypes
    )

type ReferencedMethod with
    static member Instance(visibility, flags, returnType, name, parameterTypes) =
        new MethodReference<MethodKinds.Instance>(visibility, flags, returnType, name, parameterTypes)

    static member Abstract(visibility, flags, returnType, name, parameterTypes) =
        new MethodReference<MethodKinds.Abstract>(visibility, flags, returnType, name, parameterTypes)

    static member Final(visibility, flags, returnType, name, parameterTypes) =
        new MethodReference<MethodKinds.Final>(visibility, flags, returnType, name, parameterTypes)

    static member Static(visibility, flags, returnType, name, parameterTypes) =
        new MethodReference<MethodKinds.Static>(visibility, flags, returnType, name, parameterTypes)

    static member Virtual(visibility, flags, returnType, name, parameterTypes) =
        new MethodReference<MethodKinds.Virtual>(visibility, flags, returnType, name, parameterTypes)

    static member Constructor(visibility, flags, parameterTypes) =
        new MethodReference<MethodKinds.ObjectConstructor> (
            visibility,
            flags,
            ReturnType.RVoid,
            MethodName MethodName.ctor,
            parameterTypes
        )

[<RequireQualifiedAccess>]
module ReferencedMethod =
    let inline (|Instance|Virtual|Final|Static|Abstract|Constructor|) (method: ReferencedMethod) =
        match method with
        | :? MethodReference<MethodKinds.Instance> as method' -> Instance method'
        | :? MethodReference<MethodKinds.Virtual> as method' -> Virtual method'
        | :? MethodReference<MethodKinds.Final> as method' -> Final method'
        | :? MethodReference<MethodKinds.Static> as method' -> Static method'
        | :? MethodReference<MethodKinds.Abstract> as method' -> Abstract method'
        | _ -> Constructor(method :?> MethodReference<MethodKinds.ObjectConstructor>)
