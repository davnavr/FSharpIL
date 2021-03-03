namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

// NOTE: For methods, SpecialName has to be set if RTSpecialName is set.
// NOTE: For methods, RTSpecialName and SpecialName is set when it is a ctor or cctor
type VTableLayout =
    | ReuseSlot
    | NewSlot

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type StaticMethodDefFlags<'Visibility when 'Visibility :> IFlags<MethodAttributes>> =
    { Visibility: 'Visibility
      HideBySig: bool }

    member this.Value =
        let flags = this.Visibility.Value
        if this.HideBySig
        then flags ||| MethodAttributes.HideBySig
        else flags

    interface IFlags<MethodAttributes> with member this.Value = this.Value

[<IsReadOnly; Struct>]
type InstanceMethodDefFlags =
    { Visibility: Visibility
      HideBySig: bool
      VTableLayout: VTableLayout }

    member this.Value =
        let vtable =
            match this.VTableLayout with
            | ReuseSlot -> MethodAttributes.ReuseSlot
            | NewSlot -> MethodAttributes.NewSlot
        let mutable flags = (this.Visibility :> IFlags<MethodAttributes>).Value
        if this.HideBySig then flags <- flags ||| MethodAttributes.HideBySig
        flags ||| vtable

    interface IFlags<MethodAttributes> with member this.Value = this.Value

[<IsReadOnly; Struct>]
type MethodImplFlags =
    { ForwardRef: bool
      PreserveSig: bool
      NoInlining: bool
      NoOptimization: bool }

    member this.Value =
        let mutable flags = enum<MethodImplAttributes> 0
        if this.ForwardRef then flags <- flags ||| MethodImplAttributes.ForwardRef
        if this.PreserveSig then flags <- flags ||| MethodImplAttributes.PreserveSig
        if this.NoInlining then flags <- flags ||| MethodImplAttributes.NoInlining
        if this.NoOptimization then flags <- flags ||| MethodImplAttributes.NoOptimization
        flags

    interface IFlags<MethodImplAttributes> with member this.Value = this.Value

    static member None =
        { ForwardRef = false
          PreserveSig = false
          NoInlining = false
          NoOptimization = false }

[<AbstractClass; Sealed>] type InstanceMethodFlags = class end
[<AbstractClass; Sealed>] type AbstractMethodFlags = class end
[<AbstractClass; Sealed>] type FinalMethodFlags = class end
[<AbstractClass; Sealed>] type StaticMethodFlags = class end
[<AbstractClass; Sealed>] type GlobalMethodFlags = class end

// NOTE: Constructors and Class Constructors cannot be marked CompilerControlled.
[<AbstractClass; Sealed>] type ConstructorFlags = class end
[<AbstractClass; Sealed>] type ClassConstructorFlags = class end

type MethodCallingConventions =
    | Default
    | VarArg
    // | Generic // of count: int

[<Flags>]
type internal CallingConvention =
    | HasThis = 0x20uy
    | ExplicitThis = 0x40uy
    | Default = 0uy
    | VarArg = 0x5uy
    | Generic = 0x10uy

// TODO: Have different signature types for different kinds of methods.
/// <summary>Represents a <c>MethodDefSig</c>, which captures the signature of a method or global function (II.23.2.1).</summary>
[<IsReadOnly; Struct>]
type MethodDefSignature internal (hasThis: bool, explicitThis: bool, cconv: MethodCallingConventions, retType: ReturnTypeItem, parameters: ImmutableArray<ParamItem>) =
    member _.CallingConventions = cconv
    member internal _.Flags =
        let mutable flags =
            match cconv with
            | Default -> CallingConvention.Default
            | VarArg -> CallingConvention.VarArg
        if hasThis then flags <- flags ||| CallingConvention.HasThis
        if explicitThis then flags <- flags ||| CallingConvention.ExplicitThis
        flags
    member _.ReturnType = retType
    member _.Parameters: ImmutableArray<ParamItem> = parameters

    member internal this.CheckOwner owner =
        retType.CheckOwner owner
        for param in this.Parameters do param.CheckOwner owner

// TODO: Come up with better name for this interface.
type IMethodDefSignature =
    abstract Signature: unit -> MethodDefSignature

// TODO: Rename to MethodDefRow.
/// <summary>Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<Sealed>]
type MethodDef internal (body, iflags, attr, name, signature: MethodDefSignature, paramList) =
    member _.Body: IMethodBody = body
    member _.ImplFlags: MethodImplAttributes = iflags
    member _.Flags: MethodAttributes = attr
    member _.Name: Identifier = name
    member _.Signature: MethodDefSignature = signature
    member val ParamList =
        let len = signature.Parameters.Length
        let parameters = ImmutableArray.CreateBuilder<ParamRow> len
        for i = 0 to len - 1 do
            let item = signature.Parameters.Item i
            paramList item i |> parameters.Add
        parameters.ToImmutable()

    member internal _.SkipDuplicateChecking = attr &&& MethodAttributes.MemberAccessMask = MethodAttributes.PrivateScope

    interface IEquatable<MethodDef> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.Name = other.Name && this.Signature = other.Signature

    interface IIndexValue with member this.CheckOwner owner = this.Signature.CheckOwner owner

type IMethod =
    inherit IIndexValue
    abstract Definition : unit -> MethodDef

[<AutoOpen>]
module MethodHelpers =
    let internal (|MethodDef|) (mthd: #IMethod) = mthd.Definition()

[<RequireQualifiedAccess>]
type MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile =
    | AAAAA

    interface IMethodDefSignature with member _.Signature() = failwith "uh oh signature"

[<NoComparison; CustomEquality>]
type Method<'Body, 'Flags, 'Signature when 'Body :> IMethodBody and 'Signature :> IMethodDefSignature and 'Signature : equality> =
    { Body: 'Body
      ImplFlags: MethodImplFlags
      Flags: ValidFlags<'Flags, MethodAttributes>
      MethodName: Identifier
      Signature: 'Signature
      // TODO: Add ParamRow to represent method return type, allowing custom attributes to be applied to the return type.
      ParamList: ParamItem -> int -> ParamRow }

    // TODO: Remove duplicate equality code shared with MethodDef class.
    member internal this.SkipDuplicateChecking = this.Flags.Value &&& MethodAttributes.MemberAccessMask = MethodAttributes.PrivateScope

    interface IEquatable<Method<'Body, 'Flags, 'Signature>> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.MethodName = other.MethodName && this.Signature = other.Signature

    interface IMethod with
        // TODO: See if IMethod is required to implement IIndexValue.
        member _.CheckOwner _ = ()
        member this.Definition() =
            MethodDef(this.Body, this.ImplFlags.Value, this.Flags.Value, this.MethodName, this.Signature.Signature(), this.ParamList)

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type StaticMethodSignature =
    struct
        val CallingConventions: MethodCallingConventions
        val ReturnType: ReturnTypeItem
        val Parameters: ImmutableArray<ParamItem>

        new (cconv, rtype, parameters) =
            { CallingConventions = cconv
              ReturnType = rtype
              Parameters = parameters }

        member this.Signature() =
            MethodDefSignature(false, false, this.CallingConventions, this.ReturnType, this.Parameters)

        interface IMethodDefSignature with member this.Signature() = this.Signature()
    end

// TODO: Prevent NullMethodBody from being used in non-abstract method defs.
// TODO: Create different method body types for different methods.
type InstanceMethodDef = Method<IMethodBody, InstanceMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
// TODO: Figure out how to make it so that abstract methods do not have a body.
type AbstractMethodDef = Method<NullMethodBody, AbstractMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
type FinalMethodDef = Method<IMethodBody, FinalMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
type StaticMethodDef = Method<IMethodBody, StaticMethodFlags, StaticMethodSignature>
// TODO: Prevent constructors from having generic parameters (an entry in the GenericParam table).
/// <summary>Represents a method named <c>.ctor</c>, which is an object constructor method.</summary>
type ConstructorDef = Method<IMethodBody, ConstructorFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
type ClassConstructorDef = Method<IMethodBody, ClassConstructorFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>

[<AbstractClass>]
type EntryPointSignature internal () =
    abstract Signature: unit -> MethodDefSignature
    interface IMethodDefSignature with member this.Signature() = this.Signature()

[<RequireQualifiedAccess>]
type EntryPointMethod =
    | Valid of Method<IMethodBody, StaticMethodFlags, EntryPointSignature>
    /// An entrypoint method with no restrictions on the signature, parameters, or return type. Such a signature may not be valid in implementations of the CLR.
    | Custom of StaticMethodDef

    interface IMethod with
        member this.CheckOwner owner =
            match this with
            | Valid (IndexValue method)
            | Custom (IndexValue method) ->
                IndexOwner.checkOwner owner method

        member this.Definition() =
            match this with
            | Valid (MethodDef def)
            | Custom (MethodDef def) -> def

type EntryPointIndex = TaggedIndex<EntryPointMethod, MethodDef>

// TODO: Figure out how to avoid having users type out the full name of the method type (ex: ConcreteClassMethod.Method)
[<RequireQualifiedAccess>]
type ConcreteClassMethod =
    | EntryPoint of EntryPointMethod
    | Method of InstanceMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.CheckOwner owner =
            match this with
            | EntryPoint (IndexValue method)
            | Method (IndexValue method)
            | StaticMethod (IndexValue method)
            | Constructor (IndexValue method)
            | ClassConstructor (IndexValue method)->
                IndexOwner.checkOwner owner method

        member this.Definition() =
            match this with
            | EntryPoint (MethodDef def)
            | Method (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type AbstractClassMethod =
    | EntryPoint of EntryPointMethod
    | Method of InstanceMethodDef
    | AbstractMethod of AbstractMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.CheckOwner owner =
            match this with
            | EntryPoint (IndexValue method)
            | Method (IndexValue method)
            | AbstractMethod (IndexValue method)
            | StaticMethod (IndexValue method)
            | Constructor (IndexValue method)
            | ClassConstructor (IndexValue method)->
                IndexOwner.checkOwner owner method

        member this.Definition() =
            match this with
            | EntryPoint (MethodDef def)
            | Method (MethodDef def)
            | AbstractMethod (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type SealedClassMethod =
    | EntryPoint of EntryPointMethod
    | Method of InstanceMethodDef
    | FinalMethod of FinalMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.CheckOwner owner =
            match this with
            | EntryPoint (IndexValue method)
            | Method (IndexValue method)
            | FinalMethod (IndexValue method)
            | StaticMethod (IndexValue method)
            | Constructor (IndexValue method)
            | ClassConstructor (IndexValue method)->
                IndexOwner.checkOwner owner method

        member this.Definition() =
            match this with
            | EntryPoint (MethodDef def)
            | Method (MethodDef def)
            | FinalMethod (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type StaticClassMethod =
    | EntryPoint of EntryPointMethod
    | Method of StaticMethodDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.CheckOwner owner =
            match this with
            | EntryPoint (IndexValue method)
            | Method (IndexValue method)
            | ClassConstructor (IndexValue method)->
                IndexOwner.checkOwner owner method

        member this.Definition() =
            match this with
            | EntryPoint (MethodDef def)
            | Method (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

/// <summary>
/// Error used when there is a duplicate row in the <c>MethodDef</c> table (21).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodError (method: MethodDef) =
    inherit ValidationError()
    member _.Method = method
