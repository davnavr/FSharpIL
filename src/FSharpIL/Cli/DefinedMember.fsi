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

    type [<Struct>] Instance = interface IKind
    type [<Struct>] Virtual = interface IKind
    type [<Struct>] Final = interface IKind
    type [<Struct>] Static = interface IKind
    type [<Struct>] Abstract = interface IKind

[<NoComparison; CustomEquality>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> =
    { Visibility: MemberVisibility
      Flags: MethodAttributes<'Kind>
      ReturnType: ReturnType
      MethodName: MethodName
      Parameters: ImmutableArray<Parameter> }

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IMethodDefinition

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

    type [<Struct>] Object = interface IKind

[<NoComparison; CustomEquality>]
type ConstructorDefinition<'Kind when 'Kind :> ConstructorKinds.IKind and 'Kind : struct> =
    { Visibility: MemberVisibility
      Flags: MethodAttributes<'Kind>
      Parameters: ImmutableArray<Parameter> }

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IMethodDefinition

type ObjectConstructorDef = ConstructorDefinition<ConstructorKinds.Object>

[<NoComparison; CustomEquality>]
type DefinedMethod =
    | InstanceMethod of InstanceMethodDef
    | VirtualMethod of VirtualMethodDef
    | FinalMethod of FinalMethodDef
    | StaticMethod of StaticMethodDef
    | ObjectConstructor of ObjectConstructorDef
    | ClassConstructor // of ?
    | AbstractMethod of AbstractMethodDef
    //| PInvokeMethod

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    member internal Definition: IMethodDefinition
