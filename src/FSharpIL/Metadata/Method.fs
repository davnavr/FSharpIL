namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type [<AbstractClass; Sealed>] InstanceMethodTag = class end
type [<AbstractClass; Sealed>] AbstractMethodTag = class end
type [<AbstractClass; Sealed>] FinalMethodTag = class end
type [<AbstractClass; Sealed>] StaticMethodTag = class end
type [<AbstractClass; Sealed>] ObjectConstructorTag = class end
type [<AbstractClass; Sealed>] ClassConstructorTag = class end

type VTableLayout =
    | ReuseSlot
    | NewSlot

// TODO: Consider making flags classes with optional arguments in constructors instead of records.
[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type InstanceMethodFlags = struct
    val Value: MethodAttributes

    new
        (
            visibility: Visibility,
            specialName: SpecialName,
            vTableLayout,
            [<Optional; DefaultParameterValue(false)>] hideBySig,
            [<Optional; DefaultParameterValue(false)>] isVirtual
        ) =
        let mutable flags =
            let vTableLayout' =
                match vTableLayout with
                | ReuseSlot -> MethodAttributes.ReuseSlot
                | NewSlot -> MethodAttributes.NewSlot
            let (Flags (specialName': MethodAttributes)) = specialName
            let (Flags visiblity') = visibility
            vTableLayout' ||| visiblity' ||| specialName'
        if hideBySig then flags <- flags ||| MethodAttributes.HideBySig
        if isVirtual then flags <- flags ||| MethodAttributes.Virtual
        { Value = flags }

    new (visibility, specialName) = InstanceMethodFlags(visibility, specialName, ReuseSlot)
    new (visibility) = InstanceMethodFlags(visibility, NoSpecialName)

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
    { Body: 'Body
      ImplFlags: MethodImplFlags
      Flags: ValidFlags<'Flags, MethodAttributes>
      MethodName: Identifier
      Signature: 'Signature
      // TODO: Add ParamRow to represent method return type, allowing custom attributes to be applied to the return type.
      ParamList: ParamItem -> int -> ParamRow }

    member internal this.Definition() =
        MethodDefRow (
            this.Body,
            this.ImplFlags.Value,
            this.Flags.Value,
            this.MethodName,
            this.Signature.Signature(),
            this.ParamList
        )

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type InstanceMethodSignature = struct
    val CallingConventions: MethodCallingConventions
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>

    new (cconv, rtype, parameters) = { CallingConventions = cconv; ReturnType = rtype; Parameters = parameters }
    new (rtype, parameters) = InstanceMethodSignature(Default, rtype, parameters)
    new (rtype, [<ParamArray>] parameters: ParamItem[]) = InstanceMethodSignature(rtype, parameters.ToImmutableArray())

    member internal this.Signature() =
        MethodDefSignature(true, false, this.CallingConventions, this.ReturnType, this.Parameters)

    member internal this.CheckOwner owner =
        this.ReturnType.CheckOwner owner
        for param in this.Parameters do
            param.CheckOwner owner

    interface IMethodDefSignature with
        member this.Signature() = this.Signature()
        member this.CheckOwner owner = this.CheckOwner owner
end

type InstanceMethod = Method<ConcreteMethodBody, InstanceMethodTag, InstanceMethodSignature>
type AbstractMethod = Method<NullMethodBody, AbstractMethodTag, InstanceMethodSignature>
type FinalMethod = Method<ConcreteMethodBody, FinalMethodTag, InstanceMethodSignature>

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
    new (parameters: ImmutableArray<_>) = { Parameters = parameters }
    new ([<ParamArray>] parameters: ParamItem[]) = ObjectConstructorSignature(parameters.ToImmutableArray())

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
type ObjectConstructor = Constructor<ObjectConstructorTag, ObjectConstructorSignature>
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
type ClassConstructor = Constructor<ClassConstructorTag, unit>

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type StaticMethodSignature = struct
    val private signature: InstanceMethodSignature

    new (cconv, rtype, parameters) = { signature = InstanceMethodSignature(cconv, rtype, parameters) }
    new (rtype, parameters) = StaticMethodSignature(Default, rtype, parameters)
    new (rtype, [<ParamArray>] parameters: ParamItem[]) = StaticMethodSignature(rtype, parameters.ToImmutableArray())

    member this.CallingConventions = this.signature.CallingConventions
    member this.ReturnType = this.signature.ReturnType
    member this.Parameters = this.signature.Parameters

    member internal this.Signature() =
        MethodDefSignature(false, false, this.CallingConventions, this.ReturnType, this.Parameters)

    interface IMethodDefSignature with
        member this.Signature() = this.Signature()
        member this.CheckOwner owner = this.signature.CheckOwner owner
end

type StaticMethod = Method<ConcreteMethodBody, StaticMethodTag, StaticMethodSignature>

[<AbstractClass>]
type EntryPointSignature internal () =
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    abstract CheckOwner: IndexOwner -> unit
    abstract Signature: unit -> MethodDefSignature
    interface IMethodDefSignature with
        member this.CheckOwner owner = this.CheckOwner owner
        member this.Signature() = this.Signature()

type EntryPointMethod = Method<ConcreteMethodBody, StaticMethodTag, EntryPointSignature>

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
