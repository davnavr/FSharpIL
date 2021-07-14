namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
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

[<IsReadOnly>]
type MethodReturnType = struct
    val Tag: FSharpIL.Metadata.Signatures.ReturnTypeTag
    val Type: NamedType voption

    new (tag, argType) = { Tag = tag; Type = argType }
end

[<RequireQualifiedAccess>]
module MethodReturnType =
    let Type argType = MethodReturnType(FSharpIL.Metadata.Signatures.ReturnTypeTag.Type, ValueSome argType)
    let ByRef argType = MethodReturnType(FSharpIL.Metadata.Signatures.ReturnTypeTag.ByRef, ValueSome argType)
    let TypedByRef = MethodReturnType(FSharpIL.Metadata.Signatures.ReturnTypeTag.TypedByRef, ValueNone)
    let Void = MethodReturnType(FSharpIL.Metadata.Signatures.ReturnTypeTag.Void, ValueNone)

[<AbstractClass>]
type Method =
    val HasThis: FSharpIL.Metadata.Signatures.MethodThis
    val CallingConvention: FSharpIL.Metadata.Signatures.CallingConventions
    val Name: Identifier
    val ReturnType: MethodReturnType
    val ParameterTypes: ImmutableArray<MethodParameterType>

    new (mthis, cconv, name, rtype, ptypes) =
        { HasThis = mthis
          CallingConvention = cconv
          Name = name
          ReturnType = rtype
          ParameterTypes = ptypes }

    abstract Equals: other: Method -> bool
    default this.Equals(other: Method) =
        this.Name = other.Name
        && this.ParameterTypes = other.ParameterTypes
        && this.HasThis = other.HasThis
        && this.ReturnType = other.ReturnType
        && this.CallingConvention = other.CallingConvention

    override this.Equals(obj: obj) =
        match obj with
        | :? Method as other -> this.Equals(other = other)
        | _ -> false

    override this.GetHashCode() =
        HashCode.Combine(this.HasThis, this.CallingConvention, this.Name, this.ReturnType, this.ParameterTypes)

    interface IEquatable<Method> with member this.Equals other = this.Equals other

[<AutoOpen>]
module MethodHelpers =
    type IParameterIterator<'State> = interface
        abstract Update: int32 * 'State * inref<MethodParameterType> -> unit
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
        (parameterTypes: ImmutableArray<MethodParameterType>)
        =
        let mutable gcount = 0u
        for i = 0 to parameterTypes.Length - 1 do
            let ptype = &parameterTypes.ItemRef i

            // TODO: Check if the type refers to a generic parameter in the method, but first NamedType needs to allow MVar and Var
            //gcount <- gcount + 1u

            Unchecked.defaultof<'Iter>.Update(i, state, &ptype)

        if gcount > 0u
        then FSharpIL.Metadata.Signatures.CallingConventions.Generic gcount
        else FSharpIL.Metadata.Signatures.CallingConventions.Default

[<RequireQualifiedAccess>]
module MethodKinds =
    type IKind = interface
        inherit IAttributeTag<MethodDefFlags>
        abstract MethodThis: FSharpIL.Metadata.Signatures.MethodThis
    end

    type Instance = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>

            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.HasThis
    end

    type Virtual = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Virtual

            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.HasThis
    end

    type Final = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.HasThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Virtual ||| MethodDefFlags.Final
    end

    type Static = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Static

            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.NoThis
    end

    type Abstract = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.HasThis

            member _.RequiredFlags
                with[<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Virtual ||| MethodDefFlags.Abstract
    end

    type ObjectConstructor = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.HasThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName
    end

    type ClassConstructor = struct
        interface IKind with
            member _.MethodThis
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    FSharpIL.Metadata.Signatures.MethodThis.NoThis

            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Static ||| MethodDefFlags.RTSpecialName ||| MethodDefFlags.SpecialName
    end

[<AbstractClass>]
type DefinedMethod =
    inherit Method

    val Flags: MethodDefFlags
    val ImplFlags: MethodImplFlags
    val Parameters: ImmutableArray<Parameter>

    new (iflags, flags, mthis, rtype, MethodName name, parameterTypes: ImmutableArray<_>, parameterList) =
        let mutable parameters = Array.zeroCreate parameterTypes.Length
        let cconv = checkMethodSig<MethodDefParamIterator, _> (parameters, parameterList) parameterTypes
        { inherit Method (
              mthis,
              cconv,
              name,
              rtype,
              parameterTypes
          )
          Flags = flags
          ImplFlags = iflags
          Parameters = Unsafe.As &parameters }

    override this.Equals(other: Method) =
        match other with
        | :? DefinedMethod as other' ->
            (this.Flags ||| other'.Flags &&& MethodDefFlags.MemberAccessMask <> MethodDefFlags.CompilerControlled)
            && base.Equals(other = other)
        | _ -> false

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
    let exitCodeType = MethodReturnType.Type PrimitiveType.I4
    let argsParameterTypes = ImmutableArray.Create(MethodParameterType.Type(SZArrayType PrimitiveType.String))

    let ExitWithArgs argsParamName = { ReturnExitCode = true; ArgumentsName = Some argsParamName }
    let VoidWithArgs argsParamName = { ReturnExitCode = false; ArgumentsName = Some argsParamName }
    let ExitNoArgs = { ReturnExitCode = true; ArgumentsName = None }
    let VoidNoArgs = { ReturnExitCode = false; ArgumentsName = None }

    let inline returnType kind = if kind.ReturnExitCode then exitCodeType else MethodReturnType.Void

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

[<AbstractClass>]
type ReferencedMethod =
    inherit Method

    val Visibility: ExternalVisibility

    new (visibility, mthis, rtype, MethodName name, parameterTypes: ImmutableArray<_>) = // TODO: Add parameter for VarArg parameter types.
        let cconv = checkMethodSig<EmptyParameterIterator, _> Omitted parameterTypes
        { inherit Method (
            mthis,
            cconv,
            name,
            rtype,
            parameterTypes
          )
          Visibility = visibility }

    override _.Equals(other: Method) =
        match other with
        | :? ReferencedMethod -> base.Equals other
        | _ -> false

[<Sealed>]
type MethodReference<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct>
    (
        visibility,
        returnType,
        name,
        parameterTypes
    )
    =
    inherit ReferencedMethod (
        visibility,
        Unchecked.defaultof<'Kind>.MethodThis,
        returnType,
        name,
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

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodCallTarget (owner: NamedType, method: Method) =
    member _.Owner = owner
    member _.Method = method

[<RequireQualifiedAccess>]
module MethodCallTarget =
    let Defined (tdef: DefinedType, method: DefinedMethod) = MethodCallTarget(tdef, method)
    let Referenced (tdef: ReferencedType, method: ReferencedMethod) = MethodCallTarget(tdef, method)

    let inline (|Defined|Referenced|) (target: MethodCallTarget) =
        match target.Owner with
        | :? ReferencedType as tref -> Referenced(struct(tref, target.Method :?> ReferencedMethod))
        | :? DefinedType as tdef -> Defined(struct(tdef, target.Method :?> DefinedMethod))
        | _ -> failwith "TODO: MethodSpec?"
