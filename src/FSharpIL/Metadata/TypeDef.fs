namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

type LayoutFlag =
    | AutoLayout
    /// Used as the default layout for structs by the C# and F# compilers.
    | SequentialLayout
    | ExplicitLayout

    member this.Flags =
        match this with
        | AutoLayout -> TypeAttributes.AutoLayout
        | SequentialLayout -> TypeAttributes.SequentialLayout
        | ExplicitLayout -> TypeAttributes.ExplicitLayout

type StringFormattingFlag =
    | AnsiClass
    | UnicodeClass
    | AutoClass
    // | CustomFormatClass

    member this.Flags =
        match this with
        | AnsiClass -> TypeAttributes.AnsiClass
        | UnicodeClass -> TypeAttributes.UnicodeClass
        | AutoClass -> TypeAttributes.AutoClass

[<IsReadOnly; Struct>]
type ClassFlags =
    { Layout: LayoutFlag
      SpecialName: bool
      Import: bool
      Serializable: bool
      StringFormat: StringFormattingFlag
      BeforeFieldInit: bool
      RTSpecialName: bool }

    member this.Value =
        let mutable flags =
            let layout =
                match this.Layout with
                | AutoLayout -> TypeAttributes.AutoLayout
                | SequentialLayout -> TypeAttributes.SequentialLayout
                | ExplicitLayout -> TypeAttributes.ExplicitLayout
            let stringf =
                match this.StringFormat with
                | AnsiClass -> TypeAttributes.AnsiClass
                | UnicodeClass -> TypeAttributes.UnicodeClass
                | AutoClass -> TypeAttributes.AutoClass
            layout ||| stringf
        if this.SpecialName then flags <- flags ||| TypeAttributes.SpecialName
        if this.Import then flags <- flags ||| TypeAttributes.Import
        if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
        if this.BeforeFieldInit then flags <- flags ||| TypeAttributes.BeforeFieldInit
        if this.RTSpecialName then flags <- flags ||| TypeAttributes.RTSpecialName
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

    static member None =
        { Layout = AutoLayout
          SpecialName = false
          Import = false
          Serializable = false
          StringFormat = AnsiClass
          BeforeFieldInit = false
          RTSpecialName = false }

[<AbstractClass; Sealed>] type ConcreteClassFlags = class end
[<AbstractClass; Sealed>] type AbstractClassFlags = class end
[<AbstractClass; Sealed>] type SealedClassFlags = class end
[<AbstractClass; Sealed>] type StaticClassFlags = class end

[<Struct; IsReadOnly>]
type DelegateFlags =
    { Serializable: bool }

    member this.Value =
        let mutable flags = TypeAttributes.Sealed
        if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

[<Struct; IsReadOnly>]
[<RequireQualifiedAccess>]
type InterfaceFlags =
    { Import: bool }

    member this.Value =
        let mutable flags = TypeAttributes.Abstract ||| TypeAttributes.Interface
        if this.Import then flags <- flags ||| TypeAttributes.Import
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type StructFlags = class end

type TypeFlags<'Tag> = ValidFlags<'Tag, TypeAttributes>
type TypeIndex<'Type> = TaggedIndex<'Type, TypeDefRow>

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

/// <summary>
/// Represents a row in the <see cref="T:FSharpIL.Metadata.TypeDefTable"/> (II.22.37).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.ClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.DelegateDef"/>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
/// <seealso cref="T:FSharpIL.Metadata.InterfaceDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StructDef"/>
[<Sealed>]
type TypeDefRow internal (flags, name, ns, extends, fields, methods, parent, genericParams) =
    member _.Flags: TypeAttributes = flags
    member _.TypeName: Identifier = name
    member _.TypeNamespace: string = ns
    member _.Extends: Extends = extends
    member _.FieldList: ImmutableArray<FieldRow> = fields
    member _.MethodList: ImmutableArray<MethodDef> = methods
    member _.EnclosingClass: SimpleIndex<TypeDefRow> option = parent
    member _.GenericParams: ImmutableArray<GenericParam> = genericParams

    override this.Equals obj =
        match obj with
        | :? TypeDefRow as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, ns)

    interface IEquatable<TypeDefRow> with
        member _.Equals other = ns = other.TypeNamespace && name = other.TypeName

    interface IIndexValue with
        member _.CheckOwner owner =
            match extends with
            | Extends.ConcreteClass (SimpleIndex concrete) -> IndexOwner.checkIndex owner concrete
            | Extends.AbstractClass (SimpleIndex abst) -> IndexOwner.checkIndex owner abst
            | Extends.TypeRef tref -> IndexOwner.checkIndex owner tref
            | Extends.Null -> ()

            Option.iter (IndexOwner.checkIndex owner) parent

[<RequireQualifiedAccess>]
type TypeVisibility =
    | NotPublic
    | Public
    | NestedPublic of SimpleIndex<TypeDefRow>
    | NestedPrivate of SimpleIndex<TypeDefRow>
    /// <summary>Equivalent to the C# <see langword="protected"/> keyword.</summary>
    | NestedFamily of SimpleIndex<TypeDefRow>
    /// <summary>Equivalent to the C# <see langword="internal"/> keyword.</summary>
    | NestedAssembly of SimpleIndex<TypeDefRow>
    /// <summary>Equivalent to the C# <see langword="private protected"/> keyword.</summary>
    | NestedFamilyAndAssembly of SimpleIndex<TypeDefRow>
    /// <summary>Equivalent to the C# <see langword="protected internal"/> keyword.</summary>
    | NestedFamilyOrAssembly of SimpleIndex<TypeDefRow>

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
type ClassDef<'Flags, 'Field, 'Method when 'Field :> IField and 'Method :> IMethod> =
    { /// <summary>
      /// Corresponds to the <c>VisibilityMask</c> flags of a type, as well as an entry in the <c>NestedClass</c> table if the current type is nested.
      /// </summary>
      Access: TypeVisibility
      Flags: ValidFlags<'Flags, TypeAttributes>
      ClassName: Identifier
      TypeNamespace: string
      Extends: Extends }

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
      TypeNamespace: string }

/// <summary>
/// Represents a user-defined value type, which is a <see cref="T:FSharpIL.Metadata.TypeDef"/> that derives from <see cref="T:System.ValueType"/>.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
type StructDef =
   { Access: TypeVisibility
     Flags: TypeFlags<StructFlags>
     StructName: Identifier
     TypeNamespace: string }

/// <summary>
/// Error used when there is a duplicate row in the <c>TypeDef</c> table (29, 30).
/// </summary>
/// <category>Errors</category>
type DuplicateTypeDefError (duplicate: TypeDefRow) =
    inherit ValidationError()
    member _.Type = duplicate
    override this.ToString() =
        sprintf
            "Unable to add type definition \"%O\", a type with the same namespace, name, and parent already exists"
            this.Type

[<Sealed>]
type TypeDefTable internal (owner: IndexOwner) =
    let defs = List<TypeDefRow>()
    let lookup = HashSet<TypeDefRow>()

    // TODO: Add the <Module> class used for global variables and functions, which should be the first entry.

    member _.Count = defs.Count

    // TODO: Enforce common CLS checks and warnings for types.
    member _.GetIndex(tdef: TypeDefRow) =
        IndexOwner.checkOwner owner tdef
        if lookup.Add tdef
        then SimpleIndex(owner, tdef) |> Ok
        else DuplicateTypeDefError tdef :> ValidationError |> Error

    interface IReadOnlyCollection<TypeDefRow> with
        member _.Count = defs.Count
        member _.GetEnumerator() = (defs :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (defs :> System.Collections.IEnumerable).GetEnumerator()
