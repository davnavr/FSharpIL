namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type VTableLayout =
    | ReuseSlot
    | NewSlot

// TODO: Consider making flags classes with optional arguments in constructors instead of records.
[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type InstanceMethodFlags = struct
    val Value: MethodAttributes

    new (visibility: Visibility, vTableLayout, specialName: SpecialName, hideBySig) =
        let mutable flags =
            let vTableLayout' =
                match vTableLayout with
                | ReuseSlot -> MethodAttributes.ReuseSlot
                | NewSlot -> MethodAttributes.NewSlot
            let (Flags (specialName': MethodAttributes)) = specialName
            let (Flags visiblity') = visibility
            vTableLayout' ||| visiblity' ||| specialName'
        if hideBySig then flags <- flags ||| MethodAttributes.HideBySig
        { Value = flags }

    new (visibility, vTableLayout, specialName) = InstanceMethodFlags(visibility, vTableLayout, specialName, false)
    new (visibility, vTableLayout) = InstanceMethodFlags(visibility, vTableLayout, NoSpecialName)

    interface IFlags<MethodAttributes> with member this.Value = this.Value
end

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type StaticMethodFlags<'Visibility when 'Visibility :> IFlags<MethodAttributes>> = struct
    val Value: MethodAttributes

    new (visibility: 'Visibility, specialName: SpecialName, [<Optional; DefaultParameterValue(false)>] hideBySig) =
        let mutable flags =
            let (Flags (specialName': MethodAttributes)) = specialName
            visibility.Value ||| specialName'
        if hideBySig then flags <- flags ||| MethodAttributes.HideBySig
        { Value = flags }

    new (visibility) = StaticMethodFlags(visibility, NoSpecialName)

    interface IFlags<MethodAttributes> with member this.Value = this.Value
end

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

[<IsReadOnly>]
type ConstructorFlags = struct
    val Visibility: Visibility
    val HideBySig: bool

    new (visibility, [<Optional; DefaultParameterValue(false)>] hideBySig) =
        { Visibility = visibility; HideBySig = hideBySig }

    interface IFlags<MethodAttributes> with
        member this.Value =
            let mutable flags = (this.Visibility :> IFlags<MethodAttributes>).Value
            if this.HideBySig then flags <- flags ||| MethodAttributes.HideBySig
            flags
end

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type ObjectConstructorSignature = struct
    val Parameters: ImmutableArray<ParamItem>
    new (parameters) = { Parameters = parameters }

    member internal this.CheckOwner owner =
        for param in this.Parameters do param.CheckOwner owner

    member internal this.Signature() =
        MethodDefSignature(true, false, MethodCallingConventions.Default, ReturnTypeItem ReturnTypeVoid.Item, this.Parameters)

    interface IMethodDefSignature with
        member this.CheckOwner owner = this.CheckOwner owner
        member this.Signature() = this.Signature()
end

[<IsReadOnly>]
type Constructor<'Flags, 'Signature> = struct
    val Body: ConcreteMethodBody
    val ImplFlags: MethodImplFlags
    val Flags: ValidFlags<'Flags, MethodAttributes>
    val Signature: 'Signature
    val ParamList: ParamItem -> int -> ParamRow // TODO: Remove Paramlist and move it to ObjectConstructor.

    new (body, implFlags, flags, signature, paramList) =
        { Body = body
          ImplFlags = implFlags
          Flags = flags
          Signature = signature
          ParamList = paramList }
end

// TODO: Prevent constructors from having generic parameters (an entry in the GenericParam table).
// NOTE: Constructors and Class Constructors cannot be marked CompilerControlled.
/// <summary>Represents a method named <c>.ctor</c>, which is an object constructor method.</summary>
[<Sealed>]
type ObjectConstructor (method: Constructor<ObjectConstructor, ObjectConstructorSignature>) =
    member private _.Definition() =
        MethodDefRow (
            method.Body,
            method.ImplFlags.Value,
            method.Flags.Value,
            Identifier.ofStr ".ctor",
            method.Signature.Signature(),
            method.ParamList
        )

    interface IIndexValue with member _.CheckOwner owner = method.Signature.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member this.Definition() = this.Definition()
    interface IMethod<AbstractClassDef> with member this.Definition() = this.Definition()
    interface IMethod<SealedClassDef> with member this.Definition() = this.Definition()

// TODO: Prevent a ClassConstructor from appearing more than once.
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
[<Sealed>]
type ClassConstructor (method: Constructor<ClassConstructor, unit>) =
    member private _.Definition() =
        MethodDefRow (
            method.Body,
            method.ImplFlags.Value,
            method.Flags.Value,
            Identifier.ofStr ".cctor",
            MethodDefSignature(
                false,
                false,
                MethodCallingConventions.Default,
                ReturnTypeItem ReturnTypeVoid.Item,
                ImmutableArray.Empty
            ),
            method.ParamList
        )

    interface IIndexValue with member _.CheckOwner _ = ()
    interface IMethod<ConcreteClassDef> with member this.Definition() = this.Definition()
    interface IMethod<AbstractClassDef> with member this.Definition() = this.Definition()
    interface IMethod<SealedClassDef> with member this.Definition() = this.Definition()
    interface IMethod<StaticClassDef> with member this.Definition() = this.Definition()

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type StaticMethodSignature = struct
    val CallingConventions: MethodCallingConventions
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>

    new (cconv, rtype, parameters) = { CallingConventions = cconv; ReturnType = rtype; Parameters = parameters }

    member internal this.Signature() =
        MethodDefSignature(false, false, this.CallingConventions, this.ReturnType, this.Parameters)

    interface IMethodDefSignature with
        member this.Signature() = this.Signature()
        member this.CheckOwner owner =
            this.ReturnType.CheckOwner owner
            for param in this.Parameters do
                param.CheckOwner owner
end

[<Sealed>]
type StaticMethod (method: Method<ConcreteMethodBody, StaticMethod, StaticMethodSignature>) =
    interface IIndexValue with member _.CheckOwner owner = method.CheckOwner owner
    interface IMethod<ConcreteClassDef> with member _.Definition() = method.Definition()
    interface IMethod<AbstractClassDef> with member _.Definition() = method.Definition()
    interface IMethod<SealedClassDef> with member _.Definition() = method.Definition()
    interface IMethod<StaticClassDef> with member _.Definition() = method.Definition()

    // NOTE: Static methods should be allowed in Interfaces

    interface IMethod<StructDef> with member _.Definition() = method.Definition()

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
