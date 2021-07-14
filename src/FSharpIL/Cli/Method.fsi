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
    val Type: NamedType voption
end

[<RequireQualifiedAccess>]
module MethodReturnType =
    val Type : argType: NamedType -> MethodReturnType
    val ByRef : argType: NamedType -> MethodReturnType
    val TypedByRef : MethodReturnType
    val Void : MethodReturnType

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

[<AbstractClass>]
type DefinedMethod =
    inherit Method

    val Flags: MethodDefFlags
    val ImplFlags: MethodImplFlags
    val Parameters: ImmutableArray<Parameter>

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
    member Kind: EntryPointKind
end

[<AbstractClass>]
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

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodCallTarget = // TODO: Prevent calling of default constructor?
    member Owner: NamedType
    member Method: Method

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

[<RequireQualifiedAccess>]
module MethodCallTarget =
    val internal Defined: DefinedType * DefinedMethod -> MethodCallTarget
    val internal Referenced: ReferencedType * ReferencedMethod -> MethodCallTarget

    //val inline (|Defined|Referenced|Specification|)
    val inline (|Defined|Referenced|): MethodCallTarget -> Choice<struct(DefinedType * DefinedMethod), struct(ReferencedType * ReferencedMethod)>
