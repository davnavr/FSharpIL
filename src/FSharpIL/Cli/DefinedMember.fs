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

type internal IMethodDefinition = interface
    inherit IEquatable<IMethodDefinition>

    abstract Visibility: MemberVisibility
    abstract ReturnType: ReturnType
    abstract MethodName: MethodName
    abstract Parameters: ImmutableArray<Parameter>
end

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
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = HasThis
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Virtual ||| MethodDefFlags.Final
    end

    type Static = struct
        interface IKind with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodDefFlags.Static
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = NoThis
    end

    type Abstract = struct
        interface IKind with
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = HasThis
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                    MethodDefFlags.Virtual ||| MethodDefFlags.Abstract
    end

[<AutoOpen>]
module MethodDefinitionHelpers =
    let inline (|MethodDefinition|) (mdef: #IMethodDefinition) = mdef :> IMethodDefinition

    let methodDefinitionEquals (current: #IMethodDefinition) (other: IMethodDefinition) =
        if current.Visibility = MemberVisibility.CompilerControlled || other.Visibility = MemberVisibility.CompilerControlled
        then false
        else
            current.MethodName = other.MethodName &&
            current.ReturnType = other.ReturnType &&
            current.Parameters = other.Parameters

    let inline methodDefinitionHash (def: #IMethodDefinition) = HashCode.Combine(def.MethodName, def.ReturnType, hash def.Parameters)

    let inline equalsMethodDefinition (current: #IMethodDefinition) (obj: obj) =
        match obj with
        | :? IMethodDefinition as other -> current.Equals other
        | _ -> false

[<NoComparison; CustomEquality>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> =
    { Visibility: MemberVisibility
      Flags: MethodAttributes<'Kind>
      ReturnType: ReturnType
      MethodName: MethodName
      Parameters: ImmutableArray<Parameter> }

    override this.Equals obj = equalsMethodDefinition this obj
    override this.GetHashCode() = methodDefinitionHash this

    interface IMethodDefinition with
        member this.Equals other = methodDefinitionEquals this other
        member this.MethodName = this.MethodName
        member this.Parameters = this.Parameters
        member this.ReturnType = this.ReturnType
        member this.Visibility = this.Visibility

type InstanceMethodDef = MethodDefinition<MethodKinds.Instance>
type VirtualMethodDef = MethodDefinition<MethodKinds.Virtual>
type FinalMethodDef = MethodDefinition<MethodKinds.Final>
type StaticMethodDef = MethodDefinition<MethodKinds.Static>
type AbstractMethodDef = MethodDefinition<MethodKinds.Abstract>

[<RequireQualifiedAccess>]
module ConstructorKinds =
    type IKind = interface
        inherit MethodKinds.IKind
        abstract MethodName: Identifier
    end

    type Object = struct
        interface IKind with
            member _.MethodName with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = MethodName.ctor
            member _.MethodThis with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = HasThis
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>
    end

[<NoComparison; CustomEquality>]
type ConstructorDefinition<'Kind when 'Kind :> ConstructorKinds.IKind and 'Kind : struct> =
    { Visibility: MemberVisibility
      Flags: MethodAttributes<'Kind>
      Parameters: ImmutableArray<Parameter> }

    override this.Equals obj = equalsMethodDefinition this obj
    override this.GetHashCode() = methodDefinitionHash this

    interface IMethodDefinition with
        member this.Equals other = methodDefinitionEquals this other
        member _.MethodName = MethodName Unchecked.defaultof<'Kind>.MethodName
        member this.Parameters = this.Parameters
        member _.ReturnType = ReturnType.Void ImmutableArray.Empty
        member this.Visibility = this.Visibility

type ObjectConstructorDef = ConstructorDefinition<ConstructorKinds.Object>

[<RequireQualifiedAccess>]
module ClassConstructor =
    let instance =
        { new IMethodDefinition with
            member this.Equals other = methodDefinitionEquals this other
            member _.ReturnType = ReturnType.Void ImmutableArray.Empty
            member _.MethodName = MethodName MethodName.cctor
            member _.Parameters = ImmutableArray.Empty
            member _.Visibility = MemberVisibility.Private }

[<NoComparison; CustomEquality>]
type DefinedMethod =
    | InstanceMethod of InstanceMethodDef
    | VirtualMethod of VirtualMethodDef
    | FinalMethod of FinalMethodDef
    | StaticMethod of StaticMethodDef
    | ObjectConstructor of ObjectConstructorDef
    | ClassConstructor
    | AbstractMethod of AbstractMethodDef
    //| PInvokeMethod

    member this.Definition =
        match this with
        | InstanceMethod(MethodDefinition def)
        | VirtualMethod(MethodDefinition def)
        | FinalMethod(MethodDefinition def)
        | StaticMethod(MethodDefinition def)
        | AbstractMethod(MethodDefinition def)
        | ObjectConstructor(MethodDefinition def) -> def
        | ClassConstructor -> ClassConstructor.instance

    override this.GetHashCode() = this.Definition.GetHashCode()
    override this.Equals obj = equalsMethodDefinition this.Definition obj
