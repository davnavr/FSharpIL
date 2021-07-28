namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
type MethodName =
    val internal Name: Identifier
    internal new : name: Identifier -> MethodName
    override ToString: unit -> string

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

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ReturnType =
    | T of CliType
    | ByRef of modifiers: ImmutableArray<ModifierType> * CliType
    | TypedByRef of modifiers: ImmutableArray<ModifierType>
    | Void of modifiers: ImmutableArray<ModifierType>

    interface IEquatable<ReturnType>

[<RequireQualifiedAccess>]
module ReturnType =
    val TypedByRef' : ReturnType
    val Void' : ReturnType

[<Class>]
type Method =
    val HasThis: MethodThis
    val CallingConvention: CallingConventions
    val Name: Identifier
    val ReturnType: ReturnType
    val ParameterTypes: ImmutableArray<ParameterType>

    internal new:
        hasThis: MethodThis *
        callingConvention: CallingConventions *
        name: Identifier *
        returnType: ReturnType *
        parameterTypes: ImmutableArray<ParameterType> -> Method

    /// <summary>Gets a value indicating whether or not the method has a return value.</summary>
    /// <returns>
    /// <see langword="true"/> if the method has a return value, or <see langword="false"/> if the method returns
    /// <see langword="void"/>.
    /// </returns>
    member HasReturnValue : bool

    abstract Equals: other: Method -> bool
    default Equals: other: Method -> bool

    override ToString: unit -> string
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
        methodThis: MethodThis *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> DefinedMethod

    //member Visibility: MemberVisibility

    override Equals: Method -> bool

[<RequireQualifiedAccess>]
module Method =
    [<Sealed>]
    type SignatureComparer =
        member Equals: x: #Method * y: #Method -> bool
        interface System.Collections.Generic.IEqualityComparer<Method>

    val signatureComparer : SignatureComparer

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
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Instance>

    static member Virtual:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Virtual> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Virtual>

    static member Final:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Final> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Final>

    static member Static:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Static> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Static>

    static member Abstract:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.Abstract> *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> *
        parameterList: ParameterList -> MethodDefinition<MethodKinds.Abstract>

    static member Constructor:
        visibility: MemberVisibility *
        flags: MethodAttributes<MethodKinds.ObjectConstructor> *
        parameterTypes: ImmutableArray<ParameterType> *
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
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.Instance>

    static member Virtual:
        visibility: ExternalVisibility *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.Virtual>

    static member Final:
        visibility: ExternalVisibility *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.Final>

    static member Static:
        visibility: ExternalVisibility *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.Static>

    static member Abstract:
        visibility: ExternalVisibility *
        returnType: ReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.Abstract>

    static member Constructor:
        visibility: ExternalVisibility *
        parameterTypes: ImmutableArray<ParameterType> -> MethodReference<MethodKinds.ObjectConstructor>

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
