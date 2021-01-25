namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open System.Reflection
open System.Runtime.CompilerServices

open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module internal SystemType =
    let Delegate = "System", Identifier "Delegate"
    let Enum = "System", Identifier "Enum"
    let ValueType = "System", Identifier "ValueType"

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
type ClsViolation =
    /// A violation of rule 19, which states that "CLS-compliant interfaces shall not define...fields".
    | InterfaceContainsFields of InterfaceDef

type ValidationWarning =
    /// 10
    | DuplicateAssemblyRef of AssemblyRef
    /// 1d
    | TypeRefUsesModuleResolutionScope of TypeRef

type ValidationError =
    /// Error used when a duplicate object was added to a table, or when a duplicate method or field is added to a type.
    | DuplicateValue of obj
    | MissingType of ns: string * Identifier

    override this.ToString() =
        match this with
        | DuplicateValue value -> value.GetType().Name |> sprintf "Cannot add duplicate %s to the table"
        | MissingType(ns, name) ->
            match ns with
            | "" -> string name
            | _ -> sprintf "%s.%A" ns name
            |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"

type ValidationResult<'Result> = ValidationResult<'Result, ClsViolation, ValidationWarning, ValidationError>

type internal ITable<'Value> =
    inherit IReadOnlyCollection<'Value>

    abstract Comparer : IEqualityComparer<'Value>

type Table<'Value when 'Value :> IHandleValue> internal (state: MetadataBuilderState, comparer: IEqualityComparer<'Value>) =
    let set = HashSet<'Value> comparer

    new(state: MetadataBuilderState) = Table(state, EqualityComparer.Default)

    member _.Count = set.Count

    abstract member GetHandle : 'Value -> Result<Handle<'Value>, ValidationError>
    default _.GetHandle(value: 'Value) =
        state.EnsureOwner value
        if set.Add value |> not
        then DuplicateValue value |> Error
        else state.CreateHandle value |> Ok

    member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>

    interface ITable<'Value> with
        member _.Comparer = comparer
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator

/// II.22.30
type ModuleTable =
    { // Generation
      Name: Identifier
      Mvid: Guid
      // EncId
      // EncBaseId
      }

[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type ResolutionScope =
    | Module // NOTE: Does not occur in a compressed CLI module, and should produce a warning.
    | ModuleRef // of Handle<?>
    | AssemblyRef of Handle<AssemblyRef>
    | TypeRef of Handle<TypeRef>
    | Null

/// II.22.38
[<NoComparison; CustomEquality>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: Identifier
      TypeNamespace: string }

    interface IEquatable<TypeRef> with
        member this.Equals other =
            this.ResolutionScope = other.ResolutionScope
            && this.TypeName = other.TypeName
            && this.TypeNamespace = other.TypeNamespace

    interface IHandleValue with
        member this.Handles =
            match this.ResolutionScope with
            | ResolutionScope.AssemblyRef (IHandle handle)
            | ResolutionScope.TypeRef (IHandle handle) -> handle |> Seq.singleton
            | _ -> Seq.empty

    static member Add (t: TypeRef) (state: MetadataBuilderState) = state.TypeRef.GetHandle t

[<Sealed>]
type TypeRefTable internal (state: MetadataBuilderState) =
    inherit Table<TypeRef>(state)

    let search = Dictionary<string * Identifier, TypeRef> 8

    /// <summary>
    /// Searches for a type with the specified name and namespace, with a resolution scope
    /// of <see cref="T:FSharpIL.Metadata.ResolutionScope.AssemblyRef"/>.
    /// </summary>
    member internal this.FindType((ns, name) as t) =
        match search.TryGetValue(t) with
        | (true, existing) -> state.CreateHandle existing |> Some
        | (false, _) ->
            Seq.tryPick
                (function
                | { ResolutionScope = ResolutionScope.AssemblyRef _ } as t' when t'.TypeName = name && t'.TypeNamespace = ns ->
                    search.Item <- (ns, name), t'
                    state.CreateHandle t' |> Some
                | _ -> None)
                this

    override _.GetHandle typeRef =
        let token = base.GetHandle typeRef

        match token with
        | Ok _ ->
            // TODO: Check that the name is a "valid CLS identifier".

            match typeRef.ResolutionScope with
            | ResolutionScope.Module -> TypeRefUsesModuleResolutionScope typeRef |> state.Warnings.Add
            | _ -> ()
        | _ -> ()

        token

/// <summary>
/// Specifies which type a <see cref="T:FSharpIL.Metadata.TypeDef"/> extends.
/// </summary>
[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type Extends =
    /// Extend a class that is not sealed or abstract.
    | ConcreteClass of TypeHandle<ConcreteClassDef>
    /// Extend an abstract class.
    | AbstractClass of TypeHandle<AbstractClassDef>
    /// Extends a type referenced in another assembly.
    | TypeRef of Handle<TypeRef>
    // | TypeSpec of Handle<?>
    /// <summary>
    /// Indicates that a class does not extend another class, used by <see cref="T:System.Object"/> and interfaces.
    /// </summary>
    | Null

[<RequireQualifiedAccess>]
type TypeVisibility =
    | NotPublic
    | Public
    | NestedPublic of Handle<TypeDef>
    | NestedPrivate of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected"/> keyword.</summary>
    | NestedFamily of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="internal"/> keyword.</summary>
    | NestedAssembly of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="private protected"/> keyword.</summary>
    | NestedFamilyAndAssembly of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected internal"/> keyword.</summary>
    | NestedFamilyOrAssembly of Handle<TypeDef>

    /// <summary>Retrieves the enclosing class of this nested class.</summary>
    /// <remarks>In the actual metadata, nested type information is actually stored in the NestedClass table (II.22.32).</remarks>
    member this.EnclosingClass =
        match this with
        | NotPublic
        | Public -> None
        | NestedPublic parent
        | NestedPrivate parent
        | NestedFamily parent
        | NestedAssembly parent
        | NestedFamilyAndAssembly parent
        | NestedFamilyOrAssembly parent -> Some parent

    member this.Flags =
        match this with
        | NotPublic -> TypeAttributes.NotPublic
        | Public -> TypeAttributes.Public
        | NestedPublic _ -> TypeAttributes.NestedPublic
        | NestedPrivate _ -> TypeAttributes.NestedPrivate
        | NestedFamily _ -> TypeAttributes.NestedFamily
        | NestedAssembly _ -> TypeAttributes.NestedAssembly
        | NestedFamilyAndAssembly _ -> TypeAttributes.NestedFamANDAssem
        | NestedFamilyOrAssembly _ -> TypeAttributes.NestedFamORAssem

/// <summary>
/// Represents a <see cref="T:FSharpIL.Metadata.TypeDef"/> that is neither a delegate, enumeration, interface, or user-defined value type.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.ConcreteClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.AbstractClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.SealedClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StaticClassDef"/>
type ClassDef<'Flags, 'Field, 'Method when 'Flags :> IFlags<TypeAttributes> and 'Field :> IField and 'Method :> IMethod> =
    { /// <summary>
      /// Corresponds to the <c>VisibilityMask</c> flags of a type, as well as an entry in the <c>NestedClass</c> table if the current type is nested.
      /// </summary>
      Access: TypeVisibility
      Flags: 'Flags
      ClassName: Identifier
      TypeNamespace: string
      Extends: Extends
      /// <summary>Corresponds to the fields of the type declared in the <c>Field</c> table.</summary>
      Fields: FieldList<'Field>
      Methods: MethodList<'Method> }

/// Represents a class that is not sealed or abstract.
type ConcreteClassDef = ClassDef<ConcreteClassFlags, FieldChoice, ConcreteClassMethod>
/// Represents an abstract class.
type AbstractClassDef = ClassDef<AbstractClassFlags, FieldChoice, AbstractClassMethod>
type SealedClassDef = ClassDef<SealedClassFlags, FieldChoice, SealedClassMethod>
// TODO: Remove Extends field for static classes, and make them inherit from System.Object.
/// Represents a sealed and abstract class, meaning that it can only contain static members.
type StaticClassDef = ClassDef<StaticClassFlags, StaticField, StaticClassMethod>

/// <summary>
/// Represents a delegate type, which is a <see cref="T:FSharpIL.Metadata.TypeDef"/> that derives from <see cref="T:System.Delegate"/>.
/// </summary>
type DelegateDef =
    { Access: TypeVisibility
      Flags: DelegateFlags
      DelegateName: Identifier
      TypeNamespace: string }

/// <summary>
/// Represents an enumeration type, which is a <see cref="T:FSharpIL.Metadata.TypeDef"/> that derives from <see cref="T:System.Enum"/>.
/// </summary>
type EnumDef =
  { Access: TypeVisibility
    EnumName: Identifier
    TypeNamespace: string
    Values: unit }

type InterfaceDef =
    { Access: TypeVisibility
      Flags: InterfaceFlags
      InterfaceName: Identifier
      TypeNamespace: string
      Fields: FieldList<StaticField>
      Methods: unit } // TODO: Allow static methods in interfaces, though they violate CLS rules.

/// <summary>
/// Represents a user-defined value type, which is a <see cref="T:FSharpIL.Metadata.TypeDef"/> that derives from <see cref="T:System.ValueType"/>.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
type StructDef =
   { Access: TypeVisibility
     Flags: StructFlags
     StructName: Identifier
     TypeNamespace: string
     Fields: FieldList<FieldChoice>
     Methods: unit }

[<Struct; IsReadOnly>]
type TypeHandle<'Type> = // TODO: Rename to TypeDefHandle
    internal { TypeHandle: Handle<TypeDef> }

    member this.Handle = this.TypeHandle
    member this.Item = this.TypeHandle.Item // TODO: Rename to Type

    interface IHandle with
        member this.Owner = this.TypeHandle.Owner
        member _.ValueType = typeof<'Type>

/// <summary>
/// Represents a row in the <see cref="T:FSharpIL.Metadata.TypeDefTable"/> (II.22.37).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.ClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.DelegateDef"/>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
/// <seealso cref="T:FSharpIL.Metadata.InterfaceDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StructDef"/>
[<Sealed>]
type TypeDef private (flags, name, ns, extends, fields, methods, parent) =
    member _.Flags: TypeAttributes = flags
    member _.TypeName: Identifier = name
    member _.TypeNamespace: string = ns
    member _.Extends: Extends = extends
    member _.FieldList: ImmutableArray<FieldRow> = fields
    member _.MethodList: ImmutableArray<MethodDef> = methods
    member _.EnclosingClass: Handle<TypeDef> option = parent

    override this.Equals obj =
        match obj with
        | :? TypeDef as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, ns)

    interface IEquatable<TypeDef> with
        member _.Equals other =
            ns = other.TypeNamespace && name = other.TypeName

    interface IHandleValue with
        member _.Handles =
            seq {
                match extends with
                | Extends.ConcreteClass (IHandle handle)
                | Extends.AbstractClass (IHandle handle)
                | Extends.TypeRef (IHandle handle) -> handle
                | Extends.Null -> ()

                match parent with
                | Some(IHandle parent') -> parent'
                | None -> ()
            }

    static member private GetHandle<'Type> (def: 'Type) (state: MetadataBuilderState) (row: TypeDef) =
        state.TypeDef.GetHandle row
        |> Result.map (fun def' -> { TypeHandle = def' }: TypeHandle<'Type>)

    // TODO: Enforce CLS checks and warnings.
    static member private AddClassImpl({ Flags = Flags flags } as def: ClassDef<'Flags, 'Field, 'Method>) (state: MetadataBuilderState) =
        TypeDef (
            flags ||| def.Access.Flags,
            def.ClassName,
            def.TypeNamespace,
            def.Extends,
            def.Fields.ToImmutableArray(),
            def.Methods.ToImmutableArray(),
            def.Access.EnclosingClass
        )
        |> TypeDef.GetHandle<ClassDef<'Flags, 'Field, 'Method>> def state

    static member AddClass(def: ConcreteClassDef) = TypeDef.AddClassImpl def
    static member AddClass(def: AbstractClassDef) = TypeDef.AddClassImpl def
    static member AddClass(def: SealedClassDef) = TypeDef.AddClassImpl def
    static member AddClass(def: StaticClassDef) = TypeDef.AddClassImpl def

    static member AddDelegate({ Flags = Flags flags } as def: DelegateDef) (state: MetadataBuilderState) =
        match state.FindType SystemType.Delegate with
        | Some super ->
            TypeDef (
                flags ||| def.Access.Flags,
                def.DelegateName,
                def.TypeNamespace,
                Extends.TypeRef super,
                ImmutableArray.Empty,
                ImmutableArray.Empty, // TODO: Add delegate methods.
                def.Access.EnclosingClass
            )
            |> TypeDef.GetHandle<DelegateDef> def state
        | None -> MissingType SystemType.Delegate |> Error

    static member AddEnum(def: EnumDef) (state: MetadataBuilderState) =
        match state.FindType SystemType.Enum with
        | Some super ->
            TypeDef (
                def.Access.Flags ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable,
                def.EnumName,
                def.TypeNamespace,
                Extends.TypeRef super,
                ImmutableArray.Empty, // TODO: Add enum values.
                ImmutableArray.Empty, // TODO: Add enum methods, if any.
                def.Access.EnclosingClass
            )
            |> TypeDef.GetHandle<EnumDef> def state
        | None -> MissingType SystemType.Enum |> Error

    static member AddInterface({ Flags = Flags flags } as def: InterfaceDef) (state: MetadataBuilderState) =
        let intf =
            TypeDef (
                flags ||| def.Access.Flags,
                def.InterfaceName,
                def.TypeNamespace,
                Extends.Null,
                def.Fields.ToImmutableArray(),
                ImmutableArray.Empty, //def.Methods.ToImmutableArray(), // TODO: Add interface methods.
                def.Access.EnclosingClass
            )
            |> TypeDef.GetHandle<InterfaceDef> def state
        if def.Fields.Count > 0 then InterfaceContainsFields def |> state.ClsViolations.Add
        intf

    /// <summary>Defines a value type, which is a class that inherits from <see cref="T:System.ValueType"/>.</summary>
    static member AddStruct({ Flags = Flags flags } as def: StructDef) (state: MetadataBuilderState) =
        match state.FindType SystemType.ValueType with
        | Some super ->
            TypeDef (
                flags ||| def.Access.Flags,
                def.StructName,
                def.TypeNamespace,
                Extends.TypeRef super,
                def.Fields.ToImmutableArray(),
                ImmutableArray.Empty, // def.Methods.ToImmutableArray(), // TODO: Add struct methods.
                def.Access.EnclosingClass
            )
            |> TypeDef.GetHandle<StructDef> def state
        | None -> MissingType SystemType.ValueType |> Error

[<Sealed>]
type TypeDefTable internal (owner: MetadataBuilderState) =
    let defs = Table<TypeDef> owner

    // TODO: Add the <Module> class used for global variables and functions, which should be the first entry.

    member _.Count = defs.Count

    // TODO: Enforce common CLS checks and warnings for types.
    member internal _.GetHandle(t: TypeDef): Result<Handle<TypeDef>, ValidationError> = defs.GetHandle t

    interface ITable<TypeDef> with
        member _.Comparer = (defs :> ITable<_>).Comparer
        member _.Count = defs.Count
        member _.GetEnumerator() = (defs :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (defs :> System.Collections.IEnumerable).GetEnumerator()

/// <summary>Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags, name, signature) = // TODO: How to allow different types for signature.
    member _.Flags: FieldAttributes = flags
    member _.Name: Identifier = name
    member _.Signature = signature

    member internal _.SkipDuplicateChecking = flags &&& FieldAttributes.FieldAccessMask = FieldAttributes.PrivateScope

    override this.Equals obj =
        match obj with
        | :? FieldRow as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, signature)

    interface IEquatable<FieldRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else name = other.Name && signature = other.Signature

type IField =
    abstract Row : unit -> FieldRow

type FieldList<'Field when 'Field :> IField> = MemberList<'Field, FieldRow>

type Field<'Flags, 'Signature when 'Flags :> IFlags<FieldAttributes> and 'Signature :> IHandleValue and 'Signature : equality> =
    { Flags: 'Flags
      FieldName: Identifier
      Signature: 'Signature }

    interface IField with member this.Row() = FieldRow(this.Flags.Flags, this.FieldName, ())

    interface IHandleValue with member this.Handles = this.Signature.Handles

/// <summary>Represents a non-static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
type InstanceField = Field<InstanceFieldFlags, FieldSignature>

/// <summary>Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
type StaticField = Field<StaticFieldFlags, FieldSignature>

// TODO: Come up with a better name for the type.
type FieldChoice =
    | InstanceField of InstanceField
    | StaticField of StaticField

    interface IField with
        member this.Row() =
            
            match this with
            | InstanceField (FieldRow row)
            | StaticField (FieldRow row) -> row

/// <summary>
/// Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/> defined inside of the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
type GlobalField = Field<GlobalFieldFlags, FieldSignature>

// TODO: Make FieldSet and MethodSet immutable collections?
// TODO: Make computation expressions for fields and methods.
/// <summary>Represents a set of fields owned by a <see cref="T:FSharpIL.Metadata.TypeDef"/> within the <c>Field</c> table.</summary>
[<Sealed>]
[<Obsolete>]
type FieldSet<'Field when 'Field :> IField> (capacity: int) =
    let fields = HashSet<FieldRow> capacity

    new() = FieldSet 1

    member _.Count: int = fields.Count

    member _.Add(field: 'Field) =
        let field' = field.Row()
        if fields.Add field'
        then Ok()
        else Error field

    member _.ToImmutable(): ImmutableArray<FieldRow> = fields.ToImmutableArray()

// TODO: Create better way to get Handle<MethodDef> to allow easy use when setting the entrypoint or adding custom attributes.
/// <summary>Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<Sealed>]
type MethodDef internal (body, iflags, attr, name, signature: MethodDefSignature, paramList) =
    /// <summary>Corresponds to the <c>RVA</c> column of the <c>MethodDef</c> table containing the address of the method body.</summary>
    member _.Body: MethodBody = body
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

    interface IHandleValue with
        member this.Handles = Seq.empty // TODO: Return handles used by the MethodDef.

type IMethod =
    abstract Definition : unit -> MethodDef

type MethodList<'Method when 'Method :> IMethod> = MemberList<'Method, MethodDef>

type Method<'Body, 'Flags, 'Signature when 'Flags :> IFlags<MethodAttributes> and 'Signature :> IMethodDefSignature> =
    { Body: MethodBody
      ImplFlags: MethodImplFlags
      Flags: 'Flags
      MethodName: Identifier
      Signature: 'Signature
      ParamList: ParamItem -> int -> ParamRow }

    interface IMethod with
        member this.Definition() = MethodDef(this.Body, this.ImplFlags.Flags, this.Flags.Flags, this.MethodName, this.Signature.Signature(), this.ParamList)

// TODO: Create different method body types for different methods.
type InstanceMethodDef = Method<MethodBody, InstanceMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
// TODO: Figure out how to make it so that abstract methods do not have a body.
type AbstractMethodDef = Method<MethodBody (*unit*), AbstractMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
type FinalMethodDef = Method<MethodBody, FinalMethodFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
type StaticMethodDef = Method<MethodBody, StaticMethodFlags, StaticMethodSignature>
// TODO: Prevent constructors from having generic parameters (an entry in the GenericParam table).
/// <summary>Represents a method named <c>.ctor</c>, which is an object constructor method.</summary>
type ConstructorDef = Method<MethodBody, ConstructorFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
type ClassConstructorDef = Method<MethodBody, ClassConstructorFlags, MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile>

// TODO: Figure out how to avoid having users type out the full name of the method type (ex: ConcreteClassMethod.Method)
[<RequireQualifiedAccess>]
type ConcreteClassMethod =
    | Method of InstanceMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.Definition() =
            match this with
            | Method (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type AbstractClassMethod =
    | Method of InstanceMethodDef
    | AbstractMethod of AbstractMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.Definition() =
            match this with
            | Method (MethodDef def)
            | AbstractMethod (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type SealedClassMethod =
    | Method of InstanceMethodDef
    | FinalMethod of FinalMethodDef
    | StaticMethod of StaticMethodDef
    | Constructor of ConstructorDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.Definition() =
            match this with
            | Method (MethodDef def)
            | FinalMethod (MethodDef def)
            | StaticMethod (MethodDef def)
            | Constructor (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

[<RequireQualifiedAccess>]
type StaticClassMethod =
    | Method of StaticMethodDef
    | ClassConstructor of ClassConstructorDef

    interface IMethod with
        member this.Definition() =
            match this with
            | Method (MethodDef def)
            | ClassConstructor (MethodDef def) -> def

/// Represents a parameter.
[<IsReadOnly; Struct>]
type Param =
    { Flags: ParamFlags
      /// The name of the parameter.
      ParamName: string }

/// <summary>Represents a row in the <c>Param</c> table (II.22.33)</summary>
type ParamRow =
    | Param of Param
    // | SomeParamWithADefaultValueInTheConstantTable // of Param * ?

    member this.Flags =
        match this with
        | Param { Flags = name } -> name

    member this.ParamName =
        match this with
        | Param { ParamName = name } -> name




[<RequireQualifiedAccess>]
type MemberRefParent =
    // | MethodDef // of ?
    // | ModuleRef // of ?
    // | TypeDef // of ?
    | TypeRef of Handle<TypeRef>
    // | TypeSpec // of ?

type MemberRef<'Signature when 'Signature :> IHandleValue> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

type MethodRef = MemberRef<MethodRefSignature>

// type FieldRef = 

/// <summary>Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).</summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
type MemberRefRow =
    | MethodRef of MethodRef
    // | FieldRef // of ?

    member this.MemberName =
        match this with
        | MethodRef { MemberName = name } -> name

    interface IHandleValue with
        member this.Handles =
            match this with
            | MethodRef { Signature = Handles handles } -> handles

[<IsReadOnly; Struct>]
type MemberRefHandle<'MemberRef> =
    internal { MemberRefHandle: Handle<MemberRefRow> }

    member this.Handle = this.MemberRefHandle
    member this.Member = this.MemberRefHandle.Item

    interface IHandle with
        member this.Owner = this.MemberRefHandle.Owner
        member _.ValueType = typeof<'MemberRef>

// TODO: Create an equality comparer for MemberRefRow or have MemberRefRow implement IEquatable.
[<Sealed>]
type MemberRefTable internal (owner: MetadataBuilderState) =
    let members = Table<MemberRefRow> owner

    member _.Count = members.Count

    // TODO: Enforce CLS checks.
    // NOTE: Duplicates (based on owning class, name, and signature) are allowed, but produce a warning.
    member private _.GetHandle<'MemberRef>(row: MemberRefRow) =
        members.GetHandle row
        |> Result.map (fun handle -> { MemberRefHandle = handle }: MemberRefHandle<'MemberRef>)

    member this.GetHandle(method: MethodRef) = this.GetHandle<MethodRef>(MethodRef method)
    // member this.GetHandle(field: FieldRef) = this.GetHandle<FieldRef>(FieldRef field)

    interface ITable<MemberRefRow> with
        member _.Count = members.Count
        member _.Comparer = (members :> ITable<_>).Comparer
        member _.GetEnumerator() = members.GetEnumerator()
        member _.GetEnumerator() = (members :> System.Collections.IEnumerable).GetEnumerator()

/// <summary>
/// Contains methods for adding <see cref="T:FSharpIL.Metadata.MethodRef"/> and
/// <see cref="T:FSharpIL.Metadata.FieldRef"/> instances to the <c>MemberRef</c> table.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MemberRefTable"/>
[<AbstractClass; Sealed>]
type MemberRef =
    static member AddMethod(method: MethodRef) (state: MetadataBuilderState) = state.MemberRef.GetHandle method
    // static member AddField




[<RequireQualifiedAccess>]
type CustomAttributeParent =
    // | MethodDef // of ?
    // | Field // of ?
    // | TypeRef // of ?
    | TypeDef of Handle<TypeDef>
    // | Param // of ?
    // | InterfaceImpl // of ?
    // | MemberRef of Handle<MemberRefRow>
    // | Module // of Handle<?>
    // | Permission // of ?
    // | Property // of ?
    // | Event // of ?
    // | StandAloneSig // of ?
    // | ModuleRef // of ?
    // | TypeSpec // of ?
    | Assembly of AssemblyHandle
    // | AssemblyRef // of ?
    // | File // of ?
    // | ExportedType // of ?
    // | ManifestResource // of ?
    // | GenericParam // of ?
    // | GenericParamConstraint // of ?
    // | MethodSpec // of ?

[<RequireQualifiedAccess>]
type CustomAttributeType =
    // | MethodDef // of ?
    | MemberRef of MemberRefHandle<MethodRef>

/// <summary>Represents a row in the <c>CustomAttribute</c> table (II.22.10).</summary>
type CustomAttribute =
    { Parent: CustomAttributeParent
      /// Specifies the constructor method used to create the custom attribute.
      Type: CustomAttributeType // TODO: How to ensure that the MethodRef points to a .ctor?
      Value: CustomAttributeSignature option } // TODO: How to validate signature to ensure types of fixed arguments match method signature?

    interface IHandleValue with
        member this.Handles =
            seq {
                match this.Parent with
                | CustomAttributeParent.TypeDef (IHandle parent)
                | CustomAttributeParent.Assembly (IHandle parent) -> parent

                match this.Type with
                | CustomAttributeType.MemberRef (IHandle t) -> t

                // TODO: Yield handles used in custom attribute signature.
            }

    static member Add (attr: CustomAttribute) (state: MetadataBuilderState) = state.CustomAttribute.Add attr

[<Sealed>]
type CustomAttributeTable internal (state: MetadataBuilderState) =
    let attrs = List<CustomAttribute>()

    member _.Count = attrs.Count

    member _.Add(attr: CustomAttribute) =
        state.EnsureOwner attr
        attrs.Add attr

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator



/// II.22.2
type Assembly =
    { HashAlgId: unit
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    static member Set(assembly: Assembly) (state: MetadataBuilderState) = state.SetAssembly assembly

[<Sealed>]
type AssemblyHandle internal (owner: obj) =
    interface IHandle with
        member _.Owner = owner
        member _.ValueType = typeof<Assembly>

/// II.22.5
[<CustomEquality; NoComparison>]
type AssemblyRef =
    { Version: Version
      PublicKeyOrToken: PublicKeyOrToken
      Name: AssemblyName
      Culture: AssemblyCulture
      HashValue: unit option }

    member this.Flags =
        match this.PublicKeyOrToken with
        | PublicKey _ -> 1u
        | _ -> 0u

    interface IEquatable<AssemblyRef> with
        member this.Equals other =
            this.Version = other.Version
            && this.PublicKeyOrToken = other.PublicKeyOrToken
            && this.Name = other.Name
            && this.Culture = other.Culture

    interface IHandleValue with member _.Handles = Seq.empty

    /// Adds a reference to an assembly.
    static member Add(ref: AssemblyRef) = fun (state: MetadataBuilderState) -> state.AssemblyRef.GetHandle ref
    static member Add(assembly: System.Reflection.Assembly) =
        fun (state: MetadataBuilderState) ->
            let name = assembly.GetName()
            let ref =
                { Version = name.Version
                  PublicKeyOrToken = invalidOp "What public key?"
                  Name = AssemblyName.ofStr name.Name
                  Culture = name.CultureInfo |> invalidOp "What culture?"
                  HashValue = None }
            AssemblyRef.Add ref state

[<Sealed>]
type AssemblyRefTable internal (state: MetadataBuilderState) =
    let set = HashSet()

    member _.Count = set.Count

    member _.GetHandle assemblyRef =
        if set.Add assemblyRef |> not then
            DuplicateAssemblyRef assemblyRef |> state.Warnings.Add
        state.CreateHandle assemblyRef

    interface ITable<AssemblyRef> with
        member _.Comparer = EqualityComparer<_>.Default :> IEqualityComparer<_>
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator

// II.22.32
[<Struct; IsReadOnly>]
type NestedClass =
    { NestedClass: Handle<TypeDef>
      EnclosingClass: Handle<TypeDef> }

// TODO: Have different signature types for different kinds of methods.
/// <summary>Represents a <c>MethodDefSig</c>, which captures the signature of a method or global function (II.23.2.1).</summary>
[<IsReadOnly; Struct>]
type MethodDefSignature internal (hasThis: bool, explicitThis: bool, cconv: MethodCallingConventions, retType: ReturnTypeItem, parameters: ImmutableArray<ParamItem>) =
    member _.CallingConventions: CallingConventions =
        let mutable flags =
            match cconv with
            | Default -> enum 0
            | VarArg -> CallingConventions.VarArgs
        if hasThis then flags <- flags ||| CallingConventions.HasThis
        if explicitThis then flags <- flags ||| CallingConventions.ExplicitThis
        flags
    member _.ReturnType = retType
    member _.Parameters: ImmutableArray<ParamItem> = parameters

type IMethodDefSignature =
    abstract Signature: unit -> MethodDefSignature

type MethodCallingConventions =
    | Default
    | VarArg
    // | Standard // TODO: Determine if this value is valid. See documentation for System.Reflection.CallingConventions
    // | Generic // of count: int

[<RequireQualifiedAccess>]
type MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile =
    | AAAAA

    interface IMethodDefSignature with member _.Signature() = invalidOp "uh oh signature"

type StaticMethodSignature =
    | StaticMethodSignature of MethodCallingConventions * ReturnTypeItem * ImmutableArray<ParamItem>

    interface IMethodDefSignature with
        member this.Signature() =
            let (StaticMethodSignature (cconv, rtype, parameters)) = this
            MethodDefSignature(false, false, cconv, rtype, parameters)

    // TODO: Implement IHandleValue.

/// <summary>Represents a <c>MethodRefSig</c>, which "provides the call site Signature for a method" (II.23.2.2).</summary>
[<IsReadOnly; Struct>]
type MethodRefSignature =
    { HasThis: bool
      ExplicitThis: bool
      /// <summary>Corresponds to the <c>RetType</c> item, which specifies the return type of the method.</summary>
      ReturnType: ReturnTypeItem
      Parameters: ImmutableArray<ParamItem>
      VarArgParameters: ImmutableArray<ParamItem> }

    member this.CallingConventions =
        let mutable flags = enum<CallingConventions> 0
        if this.HasThis then flags <- flags ||| CallingConventions.HasThis
        if this.ExplicitThis then flags <- flags ||| CallingConventions.ExplicitThis
        if not this.VarArgParameters.IsEmpty then flags <- flags ||| CallingConventions.VarArgs
        flags

    interface IHandleValue with
        member this.Handles = Seq.empty // TODO: Get handles used by the MethodRefSignature.

/// Captures the definition of a field or global variable (II.23.2.4).
[<IsReadOnly; Struct>]
type FieldSignature =
    { CustomMod: ImmutableArray<CustomModifier>
      FieldType: ReturnTypeItem }

    interface IHandleValue with
        member this.Handles =
            let modifiers = this.CustomMod
            let ftype = this.FieldType
            seq {
                for { ModifierType = t } in modifiers do
                    match t with
                    | TypeDef (IHandle handle)
                    | TypeRef (IHandle handle) -> handle

                // ftype // TODO: Include field type in handle validation.
            }

/// II.23.2.7
[<IsReadOnly; Struct>]
type CustomModifier =
    { Required: bool
      ModifierType: TypeDefOrRefOrSpecEncoded }

/// II.23.2.8
type TypeDefOrRefOrSpecEncoded =
    | TypeDef of Handle<TypeDef>
    | TypeRef of Handle<TypeRef>
    // TypeSpec // of ?

/// <summary>Represents a <c>Param</c> item used in signatures (II.23.2.10).</summary>
[<IsReadOnly; Struct>]
type ParamItem =
    { CustomMod: ImmutableArray<CustomModifier>
      // ByRef: unit
      // TypedByRef: unit
      /// <summary>Corresponds to the <c>Type</c> of the parameter.</summary>
      ParamType: EncodedType }

/// <summary>Represents a <c>RetType</c> used in a signature (II.23.2.11).</summary>
[<IsReadOnly; Struct>]
type ReturnTypeItem =
    { CustomMod: ImmutableArray<CustomModifier>
      ReturnType: ReturnType }

    static member Void =
        { CustomMod = ImmutableArray.Empty
          ReturnType = ReturnType.Void }

/// <summary>Represents all different possible return types encoded in a <c>RetType</c> (II.23.2.11).</summary>
/// <seealso cref="T:FSharpIL.Metadata.ReturnTypeItem"/>
[<RequireQualifiedAccess>]
type ReturnType =
    // | Type of byRef: bool * EncodedType
    // | TypedByRef // of ?
    | Void

//type ReturnType = unit

/// <summary>Represents a <c>Type</c> (II.23.2.12).</summary>
[<RequireQualifiedAccess>]
type EncodedType =
    /// <summary>Represents the <see cref="T:System.Boolean"/> type.</summary>
    | Boolean
    /// <summary>Represents the <see cref="T:System.Char"/> type.</summary>
    | Char
    /// <summary>Represents the <see cref="T:System.SByte"/> type.</summary>
    | I1
    /// <summary>Represents the <see cref="T:System.Byte"/> type.</summary>
    | U1
    /// <summary>Represents the <see cref="T:System.Int16"/> type.</summary>
    | I2
    /// <summary>Represents the <see cref="T:System.UInt16"/> type.</summary>
    | U2
    /// <summary>Represents the <see cref="T:System.Int32"/> type.</summary>
    | I4
    /// <summary>Represents the <see cref="T:System.UInt32"/> type.</summary>
    | U4
    /// <summary>Represents the <see cref="T:System.Int64"/> type.</summary>
    | I8
    /// <summary>Represents the <see cref="T:System.UInt64"/> type.</summary>
    | U8
    /// <summary>Represents the <see cref="T:System.Single"/> type.</summary>
    | R4
    /// <summary>Represents the <see cref="T:System.Double"/> type.</summary>
    | R8
    /// <summary>Represents the <see cref="T:System.IntPtr"/> type.</summary>
    | I
    /// <summary>Represents the <see cref="T:System.UIntPtr"/> type.</summary>
    | U
    | Array of EncodedType * ArrayShape
    | FunctionPointer // of MethodDefSig
    //| FunctionPointer // of MethodRefSig
    | GenericInstantiation // of ?
    //| MVAR // of ?
    /// <summary>Represents the <see cref="T:System.Object"/> type.</summary>
    | Object
    //| PTR // of ?
    //| PTR // of ?
    /// <summary>Represents the <see cref="T:System.String"/> type.</summary>
    | String
    | SZArray // of ?
    | ValueType // of TypeDefOrRefOrSpecEncoded
    //| Var // of ?

/// II.23.2.13
[<IsReadOnly; Struct>]
type ArrayShape =
    { /// Specifies the number of dimensions in the array.
      Rank: uint32 // TODO: How to prevent a value of zero?
      /// <summary>Specifies the sizes of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumSizes</c> item and <c>Size</c> items in the signature.</remarks>
      Sizes: ImmutableArray<uint32> // NOTE: The two arrays containing information for each dimension can contain less items than Rank.
      /// <summary>Specifies the lower bounds of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumLoBounds</c> item and <c>LoBound</c> items in the signature.</remarks>
      LowerBounds: ImmutableArray<int32> }

    /// Describes the shape of a single-dimensional array.
    static member OneDimension = { Rank = 1u; Sizes = ImmutableArray.Empty; LowerBounds = ImmutableArray.Empty }

/// <summary>Represents a <c>FixedArg</c> item, which stores the arguments for a custom attribute's constructor method (II.23.3).</summary>
[<RequireQualifiedAccess>]
type FixedArg =
    | Elem of Elem
    | SZArray of ImmutableArray<Elem>

[<RequireQualifiedAccess>]
type NamedArg =
    | Field // of FieldOrPropType * string * FixedArg
    | Property // of FieldOrPropType * string * FixedArg

/// <summary>Represents an <c>Elem</c> item, which is an argument in a custom attribute (II.23.3).</summary>
type Elem =
    | ValBool of bool
    | ValChar of char
    | ValR4 of float32
    | ValR8 of System.Double
    | ValI1 of int8
    | ValU1 of uint8
    | ValI2 of int16
    | ValU2 of uint16
    | ValI4 of int32
    | ValU4 of uint32
    | ValI8 of int64
    | ValU8 of uint64
    // | ValEnum // of SomehowGetTheEnumUnderlyingType?
    | SerString of string
    // | SerStringType // of SomehowGetTheCanonicalNameOfType.
    // | BoxedObject of // underlying value.

/// <summary>
/// Represents a <c>CustomAttrib</c>, which stores the arguments provided to a custom attribute's constructor,
/// as well as any values assigned to its fields or properties. (II.23.3).
/// </summary>
type CustomAttributeSignature =
    { FixedArg: ImmutableArray<FixedArg>
      NamedArg: ImmutableArray<NamedArg> }




/// <summary>Specifies the method that is called by a <see cref="T:FSharpIL.Metadata.Opcode.Call"/> instruction.</summary>
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type Callee =
    | MethodRef of MemberRefHandle<MethodRef>

type Opcode =
    /// An instruction that does nothing (III.3.51).
    | Nop
    /// An instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been tripped" (III.3.16).
    | Break
    | Call of Callee // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    // | Calli
    /// An instruction used to return from the current method (III.3.56).
    | Ret
    // TODO: Include other opcodes.

    /// An instruction that loads a literl string (III.4.16).
    | Ldstr of string

    /// Returns the bytes that make up this opcode.
    member this.Opcode =
        match this with
        | Nop -> [| 0uy |]
        | Break -> [| 1uy |]
        | Call _ -> [| 0x28uy |]

        | Ret -> [| 0x2Auy |]

        | Ldstr _ -> [| 0x72uy |]

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
// TODO: Ensure handles used in opcodes have the correct owner.
/// II.25.4
type MethodBody =
    ImmutableArray<Opcode>

[<Sealed>]
type MetadataBuilderState (mdle: ModuleTable) as this =
    let owner = Object()

    let typeDef = TypeDefTable this
    let mutable assembly = None

    let mutable entrypoint = None

    member val Header = CliHeaderFields.Default with get, set

    member this.HeaderFlags =
        let signed =
            if this.Header.StrongNameSignature.IsEmpty
            then CorFlags.None
            else CorFlags.StrongNameSigned
        CorFlags.ILOnly ||| signed

    /// The metadata version, contained in the metadata root (II.24.2.1).
    member val MetadataVersion = MetadataVersion.ofStr "v4.0.30319" with get, set

    member val internal Warnings: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ValidationWarning>()
    member val internal ClsViolations: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ClsViolation>()

    // Reserved: uint32
    member val MajorVersion: byte = 2uy
    member val MinorVersion: byte = 0uy
    // HeapSizes: byte
    // Reserved: byte
    // Valid: uint64
    // Sorted: uint64 // TODO: Figure out what Sorted is used for.
    // Rows
    /// (0x00)
    member val Module = mdle
    /// (0x01)
    member val TypeRef: TypeRefTable = TypeRefTable this
    /// (0x02)
    member _.TypeDef: TypeDefTable = typeDef
    // (0x04)
    // member Field
    // (0x06)
    // member Method
    // (0x08)
    // member Param
    // (0x09)
    // member InterfaceImpl
    /// (0x0A)
    member val MemberRef: MemberRefTable = MemberRefTable this
    // (0x0B)
    // member Constant
    /// (0x0C)
    member val CustomAttribute: CustomAttributeTable = CustomAttributeTable this
    // (0x0D)
    // member FieldMarshal
    // (0x0E)
    // member DeclSecurity
    // (0x0F)
    // member ClassLayout
    // (0x10)
    // member FieldLayout
    // (0x11)
    // member StandAloneSig
    // (0x12)
    // member EventMap
    // (0x14)
    // member Event
    // (0x15)
    // member PropertyMap
    // (0x17)
    // member Property
    // (0x18)
    // member MethodSemantics
    // (0x19)
    // member MethodImpl
    // (0x1A)
    // member ModuleRef
    // (0x1B)
    // member TypeSpec
    // (0x1C)
    // member ImplMap
    // (0x1D)
    // member FieldRva
    /// (0x20)
    member _.Assembly: Assembly option = assembly
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// (0x23)
    member val AssemblyRef: AssemblyRefTable = AssemblyRefTable this
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    // (0x26)
    // member File
    // (0x27)
    // member ExportedType
    // (0x28)
    // member ManifestResource
    /// (0x29)
    member val NestedClass =
        Seq.choose
            (fun (tdef: TypeDef) ->
                match tdef.EnclosingClass with
                | Some parent ->
                    { NestedClass = this.CreateHandle tdef
                      EnclosingClass = parent }
                    |> Some
                | _ -> None)
            typeDef
    // (0x2A)
    // member GenericParam
    // (0x2B)
    // member MethodSpec
    // (0x2C)
    // member GenericParamConstraint

    // TODO: How to specify the entrypoint in a multi-file assembly?
    /// <summary>Gets or sets the entrypoint of the assembly.</summary>
    /// <remarks>The entrypoint of the assembly is specified by the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</remarks>
    member this.EntryPoint
        with get(): Handle<MethodDef> option = entrypoint
        and set main =
            match main with
            | Some (main': Handle<_>) ->
                if main'.Owner <> owner then
                    invalidArg "main" "The specified entrypoint cannot be owned by another state."
                this.EnsureOwner main'.Item
                entrypoint <- Some main'
            | None -> entrypoint <- None

    member _.SetAssembly(assm: Assembly) =
        assembly <- Some assm
        AssemblyHandle owner

    member internal this.FindType t: Handle<_> option =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateHandle<'T> (value: 'T): Handle<'T> = Handle(owner, value)

    member internal this.CreateTable<'T when 'T : equality> (table: ITable<'T>): ImmutableTable<'T> =
        ImmutableTable(table :> IReadOnlyCollection<_>, this.CreateHandle)



    member internal _.EnsureOwner(value: IHandleValue) =
        for handle in value.Handles do
            if handle.Owner <> owner then
                sprintf
                    "A handle to a %s owned by another state was incorrectly referenced by an %s."
                    handle.ValueType.Name
                    (value.GetType().Name)
                |> invalidArg "item"

[<AutoOpen>]
module ExtraPatterns =
    let (|OptionalModifier|RequiredModifier|) (cmod: CustomModifier) =
        if cmod.Required then RequiredModifier cmod else OptionalModifier cmod

    let internal (|FieldRow|) (f: IField) = f.Row()
    let internal (|MethodDef|) (mthd: IMethod) = mthd.Definition()
