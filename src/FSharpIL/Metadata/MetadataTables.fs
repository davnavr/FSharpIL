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

    abstract member GetHandle : 'Value -> Result<Handle<'Value>, ValidationError>
    default _.GetHandle(value: 'Value) =
        state.EnsureOwner value
        if set.Add value |> not
        then DuplicateValue value |> Error
        else state.CreateHandle value |> Ok

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
type TypeHandle<'Type> =
    internal { TypeHandle: Handle<TypeDef> }

    member this.Handle = this.TypeHandle
    member this.Item = this.TypeHandle.Item

    interface IHandle with
        member this.Owner = this.TypeHandle.Owner
        member this.ValueType = this.TypeHandle.ValueType

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

    // TODO: Enforce common CLS checks and warnings for types.
    member internal _.GetHandle(t: TypeDef): Result<Handle<TypeDef>, ValidationError> = defs.GetHandle t

    interface ITable<TypeDef> with
        member _.Comparer = (defs :> ITable<_>).Comparer
        member _.Count = (defs :> ITable<_>).Count
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

/// II.25.4
type MethodBody = unit

/// <summary>Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<Sealed>]
type MethodDef internal (body, iflags, attr, name, signature, paramList) =
    /// <summary>Corresponds to the <c>RVA</c> column of the <c>MethodDef</c> table containing the method body.</summary>
    member _.Body = body: obj
    member _.ImplFlags = iflags // TODO: Open System.Runtime.CompilerServices
    member _.Flags: MethodAttributes = attr
    member _.Name: Identifier = name
    member _.Signature = signature
    //member _.ParamList = () // TODO: Iterate through params declared in signature and call the paramList function.

    member internal _.SkipDuplicateChecking = attr &&& MethodAttributes.MemberAccessMask = MethodAttributes.PrivateScope

    interface IEquatable<MethodDef> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.Name = other.Name && this.Signature = other.Signature

type IMethod =
    abstract Definition : unit -> MethodDef

type MethodList<'Method when 'Method :> IMethod> = MemberList<'Method, MethodDef>

type Method<'Body, 'Flags when 'Flags :> IFlags<MethodAttributes>> =
    { Body: 'Body
      ImplFlags: MethodImplFlags
      Flags: 'Flags
      MethodName: Identifier
      // NOTE: Parameter attributes and names stored in ParamList, method parameter types most likely stored in the signature.
      Signature: unit // MethodDefSignature // TODO: How to use generic parameters?
      ParamList: unit (*ParamTypeAndOtherInformation*) -> int -> ParamRow }

    interface IMethod with
        member this.Definition() = MethodDef(this.Body, this.ImplFlags, this.Flags.Flags, this.MethodName, this.Signature, this.ParamList)

type InstanceMethodDef = Method<MethodBody, InstanceMethodFlags> // TODO: Create different method body types for different methods.
type AbstractMethodDef = Method<unit, AbstractMethodFlags>
type FinalMethodDef = Method<MethodBody, FinalMethodFlags>
type StaticMethodDef = Method<MethodBody, StaticMethodFlags>
// TODO: Prevent constructors from having generic parameters (an entry in the GenericParam table).
/// <summary>Represents a method named <c>.ctor</c>, which is an object constructor method.</summary>
type ConstructorDef = Method<MethodBody, ConstructorFlags>
/// <summary>Represents a method named <c>.cctor</c>, which is a class constructor method.</summary>
type ClassConstructorDef = Method<MethodBody, ClassConstructorFlags>

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

/// II.22.2
type Assembly =
    { HashAlgId: unit
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

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
/// Represents the signature of a method or global function (II.23.2.1).
[<Struct; IsReadOnly>]
type MethodDefSignature internal (cconv: CallingConventions) =
    member _.CallingConvention = cconv

type MethodUnknownThingy =
    | Default
    | VarArg
    | Generic // of count: int

/// Captures the definition of a field or global variable (II.23.2.4).
[<Struct; IsReadOnly>]
type FieldSignature =
    { CustomMod: ImmutableArray<CustomModifier>
      FieldType: ReturnType }

    interface IHandleValue with
        member this.Handles =
            let modifiers = this.CustomMod
            let ftype = this.FieldType
            seq {
                for { ModifierType = t } in modifiers do
                    match t with
                    | TypeDef (IHandle handle)
                    | TypeRef (IHandle handle) -> handle

                // ftype
            }

/// II.23.2.7
[<Struct; IsReadOnly>]
type CustomModifier =
    { Required: bool
      ModifierType: TypeDefOrRefOrSpecEncoded }

/// II.23.2.8
type TypeDefOrRefOrSpecEncoded =
    | TypeDef of Handle<TypeDef>
    | TypeRef of Handle<TypeRef>
    // TypeSpec // of ?

/// II.23.2.11
[<RequireQualifiedAccess>]
type ReturnType =
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
    | R4
    | R8
    | I
    | U
    | Array // of ?
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

[<Sealed>]
type MetadataBuilderState (mdle: ModuleTable) as this =
    let owner = Object()

    let typeDef = TypeDefTable this

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
    // (0x0A)
    // member MemberRef
    // (0x0B)
    // member Constant
    // (0x0C)
    // member CustomAttribute
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
    member val Assembly: Assembly option = None with get, set // 0x20 // TODO: Figure out if None is a good default value.
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

    member internal this.FindType t: Handle<_> option =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateHandle<'T> (value: 'T): Handle<'T> = Handle(owner, value)

    member internal this.CreateTable<'T> (table: ITable<'T>): ReadOnlyDictionary<Handle<'T>, uint32> = // TODO: Use immutable dictionary instead?
        let mutable i = 0u
        let dict = Dictionary<_, _>(table.Count, HandleEqualityComparer table.Comparer)
        for value in table do
            dict.Item <- this.CreateHandle value, i
            i <- i + 1u
        ReadOnlyDictionary dict

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
