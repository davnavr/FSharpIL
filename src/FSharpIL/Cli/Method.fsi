namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly>]
type MethodName = struct
    val internal Name: Identifier
    internal new : name: Identifier -> MethodName
    override ToString: unit -> string
end

[<RequireQualifiedAccess>]
module MethodName =
    val internal cctor: Identifier
    val internal ctor: Identifier

    val tryOfIdentifier: name: Identifier -> MethodName voption

    val tryOfStr: str: string -> MethodName voption

    /// <exception cref="T:System.ArgumentException">
    /// Thrown when name is equal to the reserved names <c>.ctor</c> or <c>.cctor</c> used for constructors.
    /// </exception>
    val ofIdentifier: name: Identifier -> MethodName

    /// <exception cref="T:System.ArgumentNullException">Thrown when the input string is <see langword="null"/>.</exception>
    /// <exception cref="T:System.ArgumentException">
    /// Thrown when the input string is empty, contains a null character, or is the reserved names <c>.ctor</c> or <c>.cctor</c>
    /// used for constructors.
    /// </exception>
    val ofStr: str: string -> MethodName

    val toIdentifier : name: MethodName -> Identifier

[<AutoOpen>]
module MethodNamePatterns =
    val (|MethodName|) : name: MethodName -> Identifier

[<Sealed>]
type Method =
    member Flags: MethodDefFlags
    member Signature: MethodDefSig // TOOD: Fix, can't use Sig because it uses indices and stuff.
    member MethodName: Identifier

    internal new: MethodDefFlags * MethodDefSig * Identifier -> Method

    member Equals: Method -> bool
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<Method>

[<RequireQualifiedAccess>]
module MethodKinds =
    type IKind = interface
        inherit IAttributeTag<MethodDefFlags>
        abstract MethodThis: MethodThis
    end

    type [<Struct>] Instance = interface IKind
    type [<Struct>] Virtual = interface IKind
    type [<Struct>] Final = interface IKind
    type [<Struct>] Static = interface IKind
    type [<Struct>] Abstract = interface IKind

[<IsReadOnly>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = struct
    val Definition: Method
    val Parameters: ImmutableArray<Parameter>

    new:
        visibility: MemberVisibility *
        flags: MethodAttributes<'Kind> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParamItem> *
        parameterList: ParameterList -> MethodDefinition<'Kind>
end

type InstanceMethodDef = MethodDefinition<MethodKinds.Instance>
type VirtualMethodDef = MethodDefinition<MethodKinds.Virtual>
type FinalMethodDef = MethodDefinition<MethodKinds.Final>
type StaticMethodDef = MethodDefinition<MethodKinds.Static>
type AbstractMethodDef = MethodDefinition<MethodKinds.Abstract>

[<IsReadOnly>]
type MethodReference<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = struct
    val Reference: Method

    new:
        visibility: ExternalVisibility *
        flags: MethodAttributes<'Kind> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParamItem> -> MethodReference<'Kind>
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

    new:
        visibility: MemberVisibility *
        parameterTypes: ImmutableArray<ParamItem> *
        parameterList: ParameterList -> ConstructorDef
end

[<IsReadOnly>]
type ConstructorRef = struct
    val Reference: Method

    new: visibility: ExternalVisibility * parameterTypes: ImmutableArray<ParamItem> -> ConstructorRef
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
