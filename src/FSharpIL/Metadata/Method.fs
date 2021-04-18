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
    /// Default value used by most methods.
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
type Method<'Body, 'Flags, 'Signature when 'Body :> IMethodBody>
    (
        body: 'Body,
        flags: ValidFlags<'Flags, MethodAttributes>,
        name: Identifier,
        signature: Blob<'Signature>,
        parameters: ParamItem -> int -> ParamRow,
        implFlags: MethodImplFlags
    ) =
    new (body, flags, name, signature, parameters) = Method(body, flags, name, signature, parameters, MethodImplFlags())
    new (body, flags, name, signature) = Method(body, flags, name, signature, ParamList.noname)
    member _.Body = body
    member _.ImplFlags = implFlags
    member _.Flags = flags
    member _.MethodName = name
    member _.Signature = signature
    member _.ParamList = parameters
    member internal this.Definition() =
        MethodDefRow (
            this.Body,
            this.ImplFlags.Value,
            this.Flags.Value,
            this.MethodName,
            this.Signature.ChangeTag(),
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

    member this.Value =
        let mutable flags = (this.Visibility :> IFlags<MethodAttributes>).Value
        if this.HideBySig then flags <- flags ||| MethodAttributes.HideBySig
        flags

    interface IFlags<MethodAttributes> with member this.Value = this.Value
end

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type ObjectConstructorSignature = struct
    val Parameters: ImmutableArray<ParamItem>
    new (parameters: ImmutableArray<_>) = { Parameters = parameters }
    new (parameter: ParamItem) = ObjectConstructorSignature(ImmutableArray.Create parameter)
    new ([<ParamArray>] parameters: ParamItem[]) = ObjectConstructorSignature(parameters.ToImmutableArray())

    member internal this.Signature() =
        MethodDefSignature(true, false, MethodCallingConventions.Default, ReturnTypeItem ReturnTypeVoid.Item, this.Parameters)
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
type ObjectConstructor = Constructor<ObjectConstructorTag, Blob<ObjectConstructorSignature>>
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
end

type StaticMethod = Method<ConcreteMethodBody, StaticMethodTag, StaticMethodSignature>

[<AbstractClass>]
type EntryPointSignature internal () =
    abstract Signature: unit -> MethodDefSignature

type EntryPointMethod = Method<ConcreteMethodBody, StaticMethodTag, EntryPointSignature>

type EntryPointTokenTag =
    | Custom = 0uy
    | Valid = 1uy
    | File = 2uy

/// <summary>Represents an <c>EntryPointToken</c> (II.25.3.3).</summary>
type EntryPointToken = TaggedIndex<EntryPointTokenTag>

[<AutoOpen>]
module EntryPointToken =
    let (|ValidEntryPoint|CustomEntryPoint|EntryPointFile|) (token: EntryPointToken) =
        match token.Tag with
        | EntryPointTokenTag.Valid -> ValidEntryPoint(token.ToRawIndex<EntryPointMethod>())
        | EntryPointTokenTag.File -> EntryPointFile(token.ToRawIndex<ModuleRef>())
        | EntryPointTokenTag.Custom
        | _ -> CustomEntryPoint(token.ToRawIndex<StaticMethod>())

    /// Indicates that the entrypoint is a static method in the current assembly.
    let ValidEntryPoint (index: RawIndex<EntryPointMethod>) = index.ToTaggedIndex EntryPointTokenTag.Valid
    /// <summary>
    /// Indicates that the entrypoint is a static method in the current assembly. Its signature may not be valid in some implementations
    /// of the CLR.
    /// </summary>
    let CustomEntryPoint (index: RawIndex<StaticMethod>) = index.ToTaggedIndex EntryPointTokenTag.Custom // TODO: See if this option is needed to allow usage of Task or Task<int> in signature.
    /// Indicates that the entrypoint is defined in another module.
    let EntryPointFile (index: RawIndex<ModuleRef>) = index.ToTaggedIndex EntryPointTokenTag.File

[<Sealed>]
type MethodDefSigBlobLookup internal (lookup: BlobLookup<MethodDefSignature>) =
    member _.Count = lookup.Count
    member _.Item with get i = &lookup.[i]
    member private _.GetSignature<'Tag>(index: Blob<'Tag>) = &lookup.[index.ChangeTag()]
    member this.Item with get i = &this.GetSignature<InstanceMethodSignature> i
    member this.Item with get i = &this.GetSignature<ObjectConstructorSignature> i
    member this.Item with get i = &this.GetSignature<StaticMethodSignature> i
    member this.Item with get i = &this.GetSignature<EntryPointSignature> i

[<Sealed>]
type MethodDefSigBlobLookupBuilder internal () =
    let lookup = BlobLookupBuilder<MethodDefSignature>()
    member _.Count = lookup.Count
    member private _.TryAdd<'Tag> signature = lookup.TryAdd signature |> Result.map (fun i -> i.ChangeTag<'Tag>())

    // TODO: Use inref for methodDefSig
    member _.TryAdd signature = lookup.TryAdd signature
    member _.GetOrAdd signature = lookup.GetOrAdd signature
    member this.TryAdd(signature: InstanceMethodSignature) = signature.Signature() |> this.TryAdd<InstanceMethodSignature>
    member this.TryAdd(signature: ObjectConstructorSignature) = signature.Signature() |> this.TryAdd<ObjectConstructorSignature>
    member this.TryAdd(signature: StaticMethodSignature) = signature.Signature() |> this.TryAdd<StaticMethodSignature>
    member this.TryAdd(signature: EntryPointSignature) = signature.Signature() |> this.TryAdd<EntryPointSignature>
    member private _.GetExisting (result: Result<Blob<'Method>, Blob<MethodDefSignature>>) =
        match result with
        | Ok signature -> signature
        | Error existing -> existing.ChangeTag()
    member this.GetOrAdd(signature: InstanceMethodSignature) = this.GetExisting(this.TryAdd signature)
    member this.GetOrAdd(signature: ObjectConstructorSignature) = this.GetExisting(this.TryAdd signature)
    member this.GetOrAdd(signature: StaticMethodSignature) = this.GetExisting(this.TryAdd signature)
    member this.GetOrAdd(signature: EntryPointSignature) = this.GetExisting(this.TryAdd signature)
    member internal _.ToImmutable() = MethodDefSigBlobLookup(lookup.ToImmutable())
