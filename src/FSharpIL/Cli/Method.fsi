namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
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

[<IsReadOnly>]
type MethodReturnType = struct // TODO: Avoid code duplication with FSharpIL.Metadata.Signatures.ReturnType and MethodParameterType
    val Tag: FSharpIL.Metadata.Signatures.ReturnTypeTag
    val CustomModifiers: ImmutableArray<ModifierType>
    val Type: NamedType voption
end

(*
[<RequireQualifiedAccess>]
type MethodReturnType =
    | Type of Type: Namedtype
    | ByRef of CustomModifiers: ImmutableArray<ModifierType> * Type: Namedtype
    | TypedByRef of CustomModifiers: ImmutableArray<ModifierType>
    | Void of CustomModifiers: ImmutableArray<ModifierType>
*)

[<RequireQualifiedAccess>]
module MethodReturnType =
    val inline (|Type|ByRef|TypedByRef|Void|) :
        returnType: MethodReturnType ->
            Choice<NamedType,
                   struct(ImmutableArray<ModifierType> * NamedType),
                   ImmutableArray<ModifierType>,
                   ImmutableArray<ModifierType>>

    val Type : returnType: NamedType -> MethodReturnType
    val ByRef : modifiers: ImmutableArray<ModifierType> * returnType: NamedType -> MethodReturnType
    val TypedByRef : modifiers: ImmutableArray<ModifierType> -> MethodReturnType
    val TypedByRef' : MethodReturnType
    val Void : modifiers: ImmutableArray<ModifierType> -> MethodReturnType
    val Void' : MethodReturnType

[<AbstractClass>]
type Method =
    val HasThis: FSharpIL.Metadata.Signatures.MethodThis
    val CallingConvention: FSharpIL.Metadata.Signatures.CallingConventions
    val Name: Identifier
    val ReturnType: MethodReturnType
    val ParameterTypes: ImmutableArray<MethodParameterType>

    abstract Equals: other: Method -> bool
    default Equals: other: Method -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<Method>

[<RequireQualifiedAccess>]
module Method =
    [<Sealed>]
    type SignatureComparer = class
        interface System.Collections.Generic.IEqualityComparer<Method>
    end

    val signatureComparer : SignatureComparer

[<RequireQualifiedAccess>]
module MethodKinds =
    type IKind = interface
        inherit IAttributeTag<MethodDefFlags>
        abstract MethodThis: FSharpIL.Metadata.Signatures.MethodThis
    end

    type [<Struct>] Instance = interface IKind
    type [<Struct>] Virtual = interface IKind
    type [<Struct>] Final = interface IKind
    type [<Struct>] Static = interface IKind
    type [<Struct>] Abstract = interface IKind
    type [<Struct>] ObjectConstructor = interface IKind
    type [<Struct>] ClassConstructor = interface IKind

type DefinedMethod =
    inherit Method

    val Flags: MethodDefFlags
    val ImplFlags: MethodImplFlags
    val Parameters: ImmutableArray<Parameter>

    new:
        implFlags: MethodImplFlags *
        flags: MethodDefFlags *
        methodThis: FSharpIL.Metadata.Signatures.MethodThis *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> DefinedMethod

    //member Visibility: MemberVisibility

    override Equals: Method -> bool

[<Sealed>]
type MethodDefinition<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = class
    inherit DefinedMethod
end

[<IsReadOnly; Struct>]
type EntryPointKind =
    { /// Gets a value indicating whether or not the entry point method returns an integer exit code.
      ReturnExitCode: bool
      /// Gets the optional name of the arguments parameter of the entry point method, if the method accepts arguments.
      ArgumentsName: Identifier voption option }

    member HasArguments: bool

[<RequireQualifiedAccess>]
module EntryPointKind =
    /// An entrypoint that takes a single-dimensional array of string arguments and returns an integer exit code.
    val ExitWithArgs: argsParamName: Identifier voption -> EntryPointKind
    /// An entrypoint that takes a single-dimensional array of string arguments and returns nothing.
    val VoidWithArgs: argsParamName: Identifier voption -> EntryPointKind
    /// An entrypoint that takes no arguments and returns an integer exit code.
    val ExitNoArgs: EntryPointKind
    /// An entrypoint that takes no arguments and returns nothing.
    val VoidNoArgs: EntryPointKind

[<IsReadOnly>]
type EntryPointMethod = struct
    val Method: MethodDefinition<MethodKinds.Static>

    internal new: method: MethodDefinition<MethodKinds.Static> -> EntryPointMethod

    member Kind: EntryPointKind
end

type DefinedMethod with
    static member Instance:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Instance> *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Instance>

    static member Virtual:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Virtual> *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Virtual>

    static member Final:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Final> *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Final>

    static member Static:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Static> *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Static>

    static member Abstract:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Abstract> *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Abstract>

    static member Constructor:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.ObjectConstructor> *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.ObjectConstructor>

    static member ClassConstructor: MethodDefinition<MethodKinds.ClassConstructor>

    static member EntryPoint:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Static> *
        name: MethodName *
        kind: EntryPointKind -> EntryPointMethod

type ReferencedMethod =
    inherit Method
    val Visibility: ExternalVisibility

    //val VarArgParameterTypes // TODO: Have field for VarArg types in ReferencedMethod.

    override Equals: Method -> bool

// TODO: Also keep track of method names to allow languages to use named parameters, maybe move val Parameters: ImmutableArray<Parameter> in MethodDefinition to Method base class?
[<Sealed>]
type MethodReference<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> = class
    inherit ReferencedMethod
end

type ReferencedMethod with
    static member Instance:
        visibility: ExternalVisibility *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.Instance>

    static member Virtual:
        visibility: ExternalVisibility *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.Virtual>

    static member Final:
        visibility: ExternalVisibility *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.Final>

    static member Static:
        visibility: ExternalVisibility *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.Static>

    static member Abstract:
        visibility: ExternalVisibility *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.Abstract>

    static member Constructor:
        visibility: ExternalVisibility *
        parameterTypes: ImmutableArray<MethodParameterType> -> MethodReference<MethodKinds.ObjectConstructor>

[<RequireQualifiedAccess>]
module ReferencedMethod =
    val inline (|Instance|Virtual|Final|Static|Abstract|Constructor|):
        ReferencedMethod ->
            Choice<MethodReference<MethodKinds.Instance>,
                   MethodReference<MethodKinds.Virtual>,
                   MethodReference<MethodKinds.Final>,
                   MethodReference<MethodKinds.Static>,
                   MethodReference<MethodKinds.Abstract>,
                   MethodReference<MethodKinds.ObjectConstructor>>

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodCallTarget<'Owner, 'Method when 'Owner :> NamedType and 'Method :> Method> =
    member Owner: 'Owner
    member Method: 'Method

    internal new: owner: 'Owner * method: 'Method -> MethodCallTarget<'Owner, 'Method>

type MethodCallTarget = MethodCallTarget<NamedType, Method>

[<RequireQualifiedAccess>]
module MethodCallTarget =
    val inline (|Callee|) : target: MethodCallTarget<'Owner, 'Method> -> 'Method

    val simplify : target: MethodCallTarget<'Owner, 'Method> -> MethodCallTarget

    val inline internal convert<'Owner, 'Method1, 'Method2
        when 'Owner :> NamedType
        and 'Method1 :> Method
        and 'Method2 :> Method
        and 'Method2 : not struct> :
        target: MethodCallTarget<'Owner, 'Method1> -> MethodCallTarget<'Owner, 'Method2>

[<AutoOpen>]
module MethodCallTargetPatterns =
    val inline (|MethodCallTarget|) : target: MethodCallTarget<'Owner, 'Method> -> struct('Owner * 'Method)
