namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

type VTableLayout =
    | ReuseSlot
    | NewSlot

[<IsReadOnly; Struct>]
type InstanceMethodFlags =
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
[<StructuralComparison; StructuralEquality>]
type StaticMethodFlags<'Visibility when 'Visibility :> IFlags<MethodAttributes>> =
    { Visibility: 'Visibility
      HideBySig: bool }

    interface IFlags<MethodAttributes> with
        member this.Value =
            let flags = this.Visibility.Value
            if this.HideBySig
            then flags ||| MethodAttributes.HideBySig
            else flags

[<IsReadOnly; Struct>]
type Method<'Body, 'Flags, 'Signature when 'Body :> IMethodBody and 'Signature :> IMethodDefSignature> =
    { Body: ' Body
      ImplFlags: MethodImplFlags
      Flags: ValidFlags<'Flags, MethodAttributes>
      MethodName: Identifier
      Signature: 'Signature
      // TODO: Add ParamRow to represent method return type, allowing custom attributes to be applied to the return type.
      ParamList: ParamItem -> int -> ParamRow }

    member internal this.CheckOwner owner = this.Signature.CheckOwner owner

    member internal this.Definition() =
        MethodDefRow (
            this.Body,
            this.ImplFlags.Value,
            this.Flags.Value,
            this.MethodName,
            this.Signature.Signature(),
            this.ParamList
        )

// TODO: Figure out how to prevent user from implementing IMethod and returning invalid MethodDefRow instances.

[<Sealed>]
type InstanceMethod (method: Method<ConcreteMethodBody, InstanceMethod, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()

[<Sealed>]
type AbstractMethod (method: Method<NullMethodBody, AbstractMethod, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()

[<Sealed>]
type FinalMethod (method: Method<ConcreteMethodBody, FinalMethod, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()

// TODO: Prevent constructors from having generic parameters (an entry in the GenericParam table).
// NOTE: Constructors and Class Constructors cannot be marked CompilerControlled.
/// <summary>Represents a method named <c>.ctor</c>, which is an object constructor method.</summary>
[<Sealed>]
type Constructor (method: Method<ConcreteMethodBody, Constructor, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()

[<Sealed>]
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
type ClassConstructor (method: Method<ConcreteMethodBody, ClassConstructor, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()
    interface IMethod<StaticClassDef> with member _.Definition() = method.Definition()

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type StaticMethodSignature =
    struct
        val CallingConventions: MethodCallingConventions
        val ReturnType: ReturnTypeItem
        val Parameters: ImmutableArray<ParamItem>

        new (cconv, rtype, parameters) = { CallingConventions = cconv; ReturnType = rtype; Parameters = parameters }

        member this.Signature() = MethodDefSignature(false, false, this.CallingConventions, this.ReturnType, this.Parameters)

        interface IMethodDefSignature with
            member this.CheckOwner owner =
                this.ReturnType.CheckOwner owner
                for param in this.Parameters do
                    param.CheckOwner owner

            member this.Signature() = this.Signature()
    end

[<Sealed>]
type StaticMethod (method: Method<ConcreteMethodBody, StaticMethod, StaticMethodSignature>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()
    interface IMethod<StaticClassDef> with member _.Definition() = method.Definition()

[<AbstractClass>]
type EntryPointSignature internal () =
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    abstract CheckOwner: IndexOwner -> unit
    abstract Signature: unit -> MethodDefSignature
    interface IMethodDefSignature with
        member this.CheckOwner owner = this.CheckOwner owner
        member this.Signature() = this.Signature()

[<RequireQualifiedAccess>]
type EntryPointMethod (method: Method<ConcreteMethodBody, StaticMethod, EntryPointSignature>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()
    interface IMethod<StaticClassDef> with member _.Definition() = method.Definition()

/// <summary>Represents an <c>EntryPointToken</c> (II.25.3.3).</summary>
type EntryPointToken =
    /// Indicates that the entrypoint is a static method in the current assembly.
    | ValidEntryPoint of MethodDefIndex<EntryPointMethod>
    /// <summary>
    /// Indicates that the entrypoint is a static method in the current assembly. Its signature may not be valid in some implementations
    /// of the CLR.
    /// </summary>
    | CustomEntryPoint of MethodDefIndex<StaticMethod> // TODO: See if this option is needed to allow usage of Task or Task<int> in signature.
    /// Indicates that the entrypoint is defined in another module.
    | EntryPointFile of SimpleIndex<ModuleRef>

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | ValidEntryPoint method -> IndexOwner.checkIndex owner method.Index
            | CustomEntryPoint method -> IndexOwner.checkIndex owner method.Index
            | EntryPointFile file -> IndexOwner.checkIndex owner file
