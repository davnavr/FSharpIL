namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

open FSharpIL.Metadata

// TODO: Consider moving some table types and the MetadataBuilder class to a file below this one.

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
    /// (1d)
    | TypeRefUsesModuleResolutionScope of TypeRef

type ValidationError =
    // TODO: Have different cases for different duplicate values.
    // TODO: Consider replacing errors that occur for duplicate values with something else.
    /// Error used when a duplicate object was added to a table, or when a duplicate method or field is added to a type.
    | DuplicateValue of obj
    | DuplicateFile of File
    | MissingType of ns: string * Identifier

    override this.ToString() =
        match this with
        | DuplicateValue value -> value.GetType().Name |> sprintf "Cannot add duplicate %s"
        | DuplicateFile file -> sprintf "A file with the name %A already exists in the file table" file.FileName
        | MissingType(ns, name) ->
            match ns with
            | "" -> string name
            | _ -> sprintf "%s.%A" ns name
            |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"

/// II.22.30
type ModuleTable =
    { // Generation
      Name: Identifier
      Mvid: Guid
      // EncId
      // EncBaseId
      }

/// <summary>
/// Indicates where the target <see cref="T:FSharpIL.Metadata.TypeRef"/> is defined (II.22.38).
/// </summary>
[<RequireQualifiedAccess>]
type ResolutionScope =
    /// Indicates that "the target type is defined in the current module", and produces a warning as this value should not be used.
    | Module
    /// Indicates that the target type "is defined in another module within the same assembly as this one".
    | ModuleRef of SimpleIndex<ModuleRef>
    /// Indicates that "The target type is defined in a different assembly".
    | AssemblyRef of SimpleIndex<AssemblyRef>
    | TypeRef of SimpleIndex<TypeRef>
    | Null // of SimpleIndex<ExportedTypeRow> // TODO: How to enforce that a row exists in the ExportedType table?

/// II.22.38
[<StructuralComparison; StructuralEquality>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: Identifier
      TypeNamespace: string }

[<Sealed>]
type TypeRefTable internal (owner: obj, warnings: ImmutableArray<_>.Builder) =
    let table = MutableTable<_> owner
    let search = Dictionary<string * Identifier, TypeRef> 8

    member _.Count = table.Count

    /// <summary>
    /// Searches for a type with the specified name and namespace, with a resolution scope of <see cref="T:FSharpIL.Metadata.ResolutionScope.AssemblyRef"/>.
    /// </summary>
    member internal _.FindType((ns, name) as t) =
        match search.TryGetValue(t) with
        | (true, existing) -> SimpleIndex(owner, existing) |> Some
        | (false, _) ->
            Seq.tryPick
                (function
                | { ResolutionScope = ResolutionScope.AssemblyRef _ } as t' when t'.TypeName = name && t'.TypeNamespace = ns ->
                    search.Item <- (ns, name), t'
                    SimpleIndex(owner, t') |> Some
                | _ -> None)
                table

    member _.GetIndex typeRef =
        // TODO: Check that the name is a "valid CLS identifier".
        match typeRef.ResolutionScope with
        | ResolutionScope.Module -> TypeRefUsesModuleResolutionScope typeRef |> warnings.Add
        | _ -> ()

        table.GetIndex typeRef |> Option.defaultWith (fun() -> SimpleIndex(owner, typeRef))

    member _.GetEnumerator() = table.GetEnumerator()

    interface IReadOnlyCollection<TypeRef> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

/// <summary>
/// Specifies which type a <see cref="T:FSharpIL.Metadata.TypeDef"/> extends.
/// </summary>
[<RequireQualifiedAccess>]
type Extends =
    /// Extend a class that is not sealed or abstract.
    | ConcreteClass of TypeIndex<ConcreteClassDef>
    /// Extend an abstract class.
    | AbstractClass of TypeIndex<AbstractClassDef>
    /// Extends a type referenced in another assembly.
    | TypeRef of SimpleIndex<TypeRef>
    // | TypeSpec of SimpleIndex<?>
    /// <summary>
    /// Indicates that a class does not extend another class, used by <see cref="T:System.Object"/> and interfaces.
    /// </summary>
    | Null

[<RequireQualifiedAccess>]
type TypeVisibility =
    | NotPublic
    | Public
    | NestedPublic of SimpleIndex<TypeDef>
    | NestedPrivate of SimpleIndex<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected"/> keyword.</summary>
    | NestedFamily of SimpleIndex<TypeDef>
    /// <summary>Equivalent to the C# <see langword="internal"/> keyword.</summary>
    | NestedAssembly of SimpleIndex<TypeDef>
    /// <summary>Equivalent to the C# <see langword="private protected"/> keyword.</summary>
    | NestedFamilyAndAssembly of SimpleIndex<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected internal"/> keyword.</summary>
    | NestedFamilyOrAssembly of SimpleIndex<TypeDef>

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
// TODO: Remove Extends field for static classes, and make them inherit from System.Object if this is a requirement by ECMA-335.
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

type TypeIndex<'Type> = TaggedIndex<'Type, TypeDef>

/// <summary>
/// Represents a row in the <see cref="T:FSharpIL.Metadata.TypeDefTable"/> (II.22.37).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.ClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.DelegateDef"/>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
/// <seealso cref="T:FSharpIL.Metadata.InterfaceDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StructDef"/>
[<Sealed>]
type TypeDef internal (flags, name, ns, extends, fields, methods, parent) = // TODO: Rename to TypeDefRow.
    member _.Flags: TypeAttributes = flags
    member _.TypeName: Identifier = name
    member _.TypeNamespace: string = ns
    member _.Extends: Extends = extends
    member _.FieldList: ImmutableArray<FieldRow> = fields
    member _.MethodList: ImmutableArray<MethodDef> = methods
    member _.EnclosingClass: SimpleIndex<TypeDef> option = parent

    override this.Equals obj =
        match obj with
        | :? TypeDef as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, ns)

    interface IEquatable<TypeDef> with
        member _.Equals other =
            ns = other.TypeNamespace && name = other.TypeName

type TypeDefRow = TypeDef

[<Sealed>]
type TypeDefTable internal (owner: obj) =
    let defs = MutableTable<TypeDef> owner

    // TODO: Add the <Module> class used for global variables and functions, which should be the first entry.

    member _.Count = defs.Count

    // TODO: Enforce common CLS checks and warnings for types.
    member _.GetIndex(t: TypeDef) =
        defs.GetIndex t

    interface IReadOnlyCollection<TypeDef> with
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

[<StructuralComparison; StructuralEquality>]
type Field<'Flags, 'Signature when 'Flags :> IFlags<FieldAttributes> and 'Signature : equality> =
    { Flags: 'Flags
      FieldName: Identifier
      Signature: 'Signature }

    interface IField with member this.Row() = FieldRow(this.Flags.Flags, this.FieldName, ())

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

// TODO: Create better way to get SimpleIndex<MethodDef> to allow easy use when setting the entrypoint or adding custom attributes.
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

type IMethod =
    abstract Definition : unit -> MethodDef

type MethodList<'Method when 'Method :> IMethod> = MemberList<'Method, MethodDef>

type Method<'Body, 'Flags, 'Signature when 'Flags :> IFlags<MethodAttributes> and 'Signature :> IMethodDefSignature> =
    { Body: MethodBody
      ImplFlags: MethodImplFlags
      Flags: 'Flags
      MethodName: Identifier
      Signature: 'Signature
      // TODO: Add ParamRow to represent method return type, allowing custom attributes to be applied to the return type.
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
    | TypeRef of SimpleIndex<TypeRef>
    // | TypeSpec // of ?

type MemberRef<'Signature> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

type MethodRef = MemberRef<MethodRefSignature>

// type FieldRef = 

/// <summary>
/// Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
type MemberRefRow =
    | MethodRef of MethodRef
    // | FieldRef // of ?

    member this.Class =
        match this with
        | MethodRef { Class = parent } -> parent

    member this.MemberName =
        match this with
        | MethodRef { MemberName = name } -> name

type MemberRefIndex<'Member> = TaggedIndex<'Member, MemberRefRow>

// TODO: Create an equality comparer for MemberRefRow or have MemberRefRow implement IEquatable.
[<Sealed>]
type MemberRefTable internal (owner: obj) =
    let members = MutableTable<MemberRefRow> owner

    member _.Count = members.Count

    // TODO: Enforce CLS checks.
    // NOTE: Duplicates (based on owning class, name, and signature) are allowed, but produce a warning.
    member private _.GetIndex<'MemberRef>(row: MemberRefRow) =
        members.GetIndex row
        |> Option.map MemberRefIndex
        |> Option.defaultWith (fun() -> MemberRefIndex<'MemberRef>(owner, row))

    member this.GetIndex(method: MethodRef) = this.GetIndex<MethodRef>(MethodRef method)
    // member this.GetIndex(field: FieldRef) = this.GetIndex<FieldRef>(FieldRef field)

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator




[<RequireQualifiedAccess>]
type CustomAttributeParent =
    // | MethodDef // of ?
    // | Field // of ?
    // | TypeRef // of ?
    | TypeDef of SimpleIndex<TypeDef>
    // | Param // of ?
    // | InterfaceImpl // of ?
    // | MemberRef of SimpleIndex<MemberRefRow>
    // | Module // of ?
    // | Permission // of ?
    // | Property // of ?
    // | Event // of ?
    // | StandAloneSig // of ?
    // | ModuleRef // of ?
    // | TypeSpec // of ?
    | Assembly of AssemblyIndex
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
    | MemberRef of MemberRefIndex<MethodRef>

/// <summary>Represents a row in the <c>CustomAttribute</c> table (II.22.10).</summary>
type CustomAttribute =
    { Parent: CustomAttributeParent
      /// Specifies the constructor method used to create the custom attribute.
      Type: CustomAttributeType // TODO: How to ensure that the MethodRef points to a .ctor?
      Value: CustomAttributeSignature option } // TODO: How to validate signature to ensure types of fixed arguments match method signature?

[<Sealed>]
type CustomAttributeTable internal (owner: obj) =
    let attrs = List<CustomAttribute>()

    member _.Count = attrs.Count

    member _.Add(attr: CustomAttribute) =
        // state.EnsureOwner attr // TODO: Ensure signature reference valid things with the same owner.
        attrs.Add attr

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator




/// <seealso cref="T:FSharpIL.Metadata.FileTable"/>
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type ModuleRef =
    { /// <summary>
      /// The corresponding entry in the <c>File</c> table that allows "the CLI to locate the target module".
      /// </summary>
      File: SimpleIndex<File> }

    /// <summary>
    /// Corresponds to the <c>Name</c> column, which matches an entry in the <c>File</c> table.
    /// </summary>
    member this.Name = this.File.Value.FileName

[<Sealed>]
type ModuleRefTable internal (owner: obj) =
    let modules = HashSet<ModuleRef>()

    member _.Count = modules.Count

    member _.Add moduleRef =
        modules.Add moduleRef |> ignore
        SimpleIndex(owner, moduleRef)

    interface IReadOnlyCollection<ModuleRef> with
        member _.Count = modules.Count
        member _.GetEnumerator() = modules.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = modules.GetEnumerator() :> System.Collections.IEnumerator




/// II.22.2
type Assembly =
    { HashAlgId: unit
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

type AssemblyIndex = TaggedIndex<AssemblyRef, unit>

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

// TODO: Create new class as this shares code with ModuleRefTable
[<Sealed>]
type AssemblyRefTable internal (owner: obj) =
    let set = HashSet<AssemblyRef>()

    member _.Count = set.Count

    member _.GetIndex assemblyRef =
        set.Add assemblyRef |> ignore
        SimpleIndex(owner, assemblyRef)

    interface IReadOnlyCollection<AssemblyRef> with
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator





/// <summary>Represents a row in the <c>File</c> table (II.22.19).</summary>
[<CustomComparison; CustomEquality>]
type File =
    { /// <summary>
      /// Corresponds to the <c>Flags</c> row, indicates whether a file "is a resource file or
      /// other non-metadata-containing file" (II.23.1.6).
      /// </summary>
      ContainsMetadata: bool
      FileName: Identifier
      HashValue: byte[] }

    override this.Equals obj = (this :> IEquatable<File>).Equals(obj :?> File)
    override this.GetHashCode() = this.FileName.GetHashCode()

    interface IEquatable<File> with
        member this.Equals other = this.FileName = other.FileName

    interface IComparable with
        member this.CompareTo obj = compare this.FileName (obj :?> File).FileName

[<Sealed>]
type FileTable internal (owner: obj) =
    let set = HashSet<File>()

    member _.Count = set.Count

    member _.GetIndex file =
        if set.Add file
        then SimpleIndex(owner, file) |> Ok
        else DuplicateFile file |> Error

    interface IReadOnlyCollection<File> with
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator




// II.22.32
[<Struct; IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type NestedClass =
    { NestedClass: SimpleIndex<TypeDef>
      EnclosingClass: SimpleIndex<TypeDef> }



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

type IMethodDefSignature =
    abstract Signature: unit -> MethodDefSignature

type MethodCallingConventions =
    | Default
    | VarArg
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

/// <summary>Represents a <c>MethodRefSig</c>, which "provides the call site Signature for a method" (II.23.2.2).</summary>
[<IsReadOnly; Struct>]
type MethodRefSignature =
    { HasThis: bool
      ExplicitThis: bool
      /// <summary>Corresponds to the <c>RetType</c> item, which specifies the return type of the method.</summary>
      ReturnType: ReturnTypeItem
      Parameters: ImmutableArray<ParamItem>
      VarArgParameters: ImmutableArray<ParamItem> }

    member this.ParamCount = uint32 (this.Parameters.Length + this.VarArgParameters.Length)

    member internal this.CallingConventions =
        let mutable flags = CallingConvention.Default
        if this.HasThis then flags <- flags ||| CallingConvention.HasThis
        if this.ExplicitThis then flags <- flags ||| CallingConvention.ExplicitThis
        if not this.VarArgParameters.IsEmpty then flags <- flags ||| CallingConvention.VarArg
        flags

/// Captures the definition of a field or global variable (II.23.2.4).
[<IsReadOnly; Struct>]
type FieldSignature =
    { CustomMod: ImmutableArray<CustomModifier>
      FieldType: ReturnTypeItem }

/// II.23.2.7
[<IsReadOnly; Struct>]
type CustomModifier =
    { Required: bool
      ModifierType: TypeDefOrRefOrSpecEncoded }

/// II.23.2.8
type TypeDefOrRefOrSpecEncoded =
    | TypeDef of SimpleIndex<TypeDef>
    | TypeRef of SimpleIndex<TypeRef>
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
    /// <summary>Represents a string used as an argument in a custom attribute.</summary>
    /// <remarks>Empty strings and <see langword="null"/> strings are allowed values.</remarks>
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
    | MethodRef of MemberRefIndex<MethodRef>

/// (III.1.2.1)
type Opcode =
    /// An instruction that does nothing (III.3.51).
    | Nop
    /// An instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been tripped" (III.3.16).
    | Break
    /// An instruction that calls a method (III.3.19).
    | Call of Callee // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    | Calli // of ?
    /// An instruction used to return from the current method (III.3.56).
    | Ret
    // TODO: Include other opcodes.

    // Ldnull

    /// An instruction that loads a literal string (III.4.16).
    | Ldstr of string // TODO: How to disallow null? Maybe usage of null here is same as Ldnull?

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
// TODO: Ensure index objects used in opcodes have the correct owner.
/// II.25.4
type MethodBody =
    ImmutableArray<Opcode>

[<Sealed>]
type MetadataBuilderState (mdle: ModuleTable) =
    let owner = Object()

    let warnings = ImmutableArray.CreateBuilder<ValidationWarning>()
    let clsViolations = ImmutableArray.CreateBuilder<ClsViolation>()

    let typeDef = TypeDefTable owner
    let mutable assembly = None

    let mutable entrypoint = None

    member internal _.Owner = owner

    member val Header = CliHeaderFields.Default with get, set

    member this.HeaderFlags =
        let signed =
            if this.Header.StrongNameSignature.IsEmpty
            then CorFlags.None
            else CorFlags.StrongNameSigned
        CorFlags.ILOnly ||| signed

    /// The metadata version, contained in the metadata root (II.24.2.1).
    member val MetadataVersion = MetadataVersion.ofStr "v4.0.30319" with get, set

    member val Warnings = warnings
    member val ClsViolations = clsViolations

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
    member val TypeRef: TypeRefTable = TypeRefTable(owner, warnings)
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
    member val MemberRef: MemberRefTable = MemberRefTable owner
    // (0x0B)
    // member Constant
    /// (0x0C)
    member val CustomAttribute: CustomAttributeTable = CustomAttributeTable owner
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
    /// (0x1A)
    member val ModuleRef = ModuleRefTable owner
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
    member val AssemblyRef: AssemblyRefTable = AssemblyRefTable owner
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    /// (0x26)
    member val File = FileTable owner
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
                    { NestedClass = SimpleIndex(owner, tdef)
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
        with get(): SimpleIndex<MethodDef> option = entrypoint
        and set main =
            match main with
            | Some (main': SimpleIndex<_>) ->
                if main'.Owner <> owner then
                    invalidArg "main" "The specified entrypoint cannot be owned by another state."
                // this.EnsureOwner main'.Item // TODO: Check indices of main method is owned by this state.
                entrypoint <- Some main'
            | None -> entrypoint <- None

    member _.SetAssembly(assm: Assembly) =
        assembly <- Some assm
        AssemblyIndex(owner, ())

    member internal this.FindType t: SimpleIndex<_> option =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateTable table = ImmutableTable(table, fun item -> SimpleIndex(owner, item))

[<AutoOpen>]
module ExtraPatterns =
    let internal (|FieldRow|) (f: IField) = f.Row()
    let internal (|MethodDef|) (mthd: IMethod) = mthd.Definition()
