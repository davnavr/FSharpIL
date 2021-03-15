namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

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

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type ClassFlags = struct
    val Value: TypeAttributes

    new
        (
            layout,
            stringFormat,
            [<Optional; DefaultParameterValue(false)>] specialName,
            [<Optional; DefaultParameterValue(false)>] import,
            [<Optional; DefaultParameterValue(false)>] serializable,
            [<Optional; DefaultParameterValue(false)>] beforeFieldInit,
            [<Optional; DefaultParameterValue(false)>] rtSpecialName
        ) =
        let mutable flags =
            let layout' =
                match layout with
                | AutoLayout -> TypeAttributes.AutoLayout
                | SequentialLayout -> TypeAttributes.SequentialLayout
                | ExplicitLayout -> TypeAttributes.ExplicitLayout
            let stringf =
                match stringFormat with
                | AnsiClass -> TypeAttributes.AnsiClass
                | UnicodeClass -> TypeAttributes.UnicodeClass
                | AutoClass -> TypeAttributes.AutoClass
            layout' ||| stringf
        if specialName then flags <- flags ||| TypeAttributes.SpecialName
        if import then flags <- flags ||| TypeAttributes.Import
        if serializable then flags <- flags ||| TypeAttributes.Serializable
        if beforeFieldInit then flags <- flags ||| TypeAttributes.BeforeFieldInit
        if rtSpecialName then flags <- flags ||| TypeAttributes.RTSpecialName
        { Value = flags }

    interface IFlags<TypeAttributes> with member this.Value = this.Value
end

[<AbstractClass; Sealed>] type ConcreteClassTag = class end
[<AbstractClass; Sealed>] type AbstractClassTag = class end
[<AbstractClass; Sealed>] type SealedClassTag = class end
[<AbstractClass; Sealed>] type StaticClassTag = class end

// TODO: Make these other TypeDef flag types normal structs.
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
type TypeDefIndex<'Type> = TaggedIndex<'Type, TypeDefRow>

/// <summary>
/// Specifies which type is extended by a <c>TypeDef</c>.
/// </summary>
[<RequireQualifiedAccess>]
type Extends =
    /// Extend a class that is not sealed or abstract.
    | ConcreteClass of TypeDefIndex<ConcreteClassDef>
    /// Extend an abstract class.
    | AbstractClass of TypeDefIndex<AbstractClassDef>
    /// Extends a type referenced in another assembly.
    | TypeRef of SimpleIndex<TypeRef>
    // | TypeSpec of SimpleIndex<?>
    /// <summary>
    /// Indicates that a class does not extend another class, used by <see cref="T:System.Object"/> and interfaces.
    /// </summary>
    | Null

/// <summary>Represents a row in the <c>TypeDef</c> table (II.22.37).</summary>
/// <seealso cref="T:FSharpIL.Metadata.ClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.DelegateDef"/>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
/// <seealso cref="T:FSharpIL.Metadata.InterfaceDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StructDef"/>
[<Sealed>]
type TypeDefRow internal (flags, name, ns, extends, parent) =
    member _.Flags: TypeAttributes = flags
    member _.TypeName: Identifier = name
    member _.TypeNamespace: string = ns
    member _.Extends: Extends = extends
    member _.EnclosingClass: SimpleIndex<TypeDefRow> option = parent

    override this.Equals obj =
        match obj with
        | :? TypeDefRow as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, ns)

    member internal _.GetFullName() =
        if ns.Length > 0
        then sprintf "'%s.%O'" ns name
        else sprintf "'%O'" name

    override this.ToString() =
        let visibility =
            match flags &&& TypeAttributes.VisibilityMask with
            | TypeAttributes.Public -> "public"
            | TypeAttributes.NestedPublic -> "nested public"
            | TypeAttributes.NestedPrivate -> "nested private"
            | TypeAttributes.NestedFamily  -> "nested family"
            | TypeAttributes.NestedAssembly  -> "nested assembly"
            | TypeAttributes.NestedFamANDAssem  -> "nested famandassem"
            | TypeAttributes.NestedFamORAssem  -> "nested famorassem"
            | _ -> "private"
        let layout = 
            match flags &&& TypeAttributes.LayoutMask with
            | TypeAttributes.SequentialLayout -> "sequential"
            | TypeAttributes.ExplicitLayout -> "explicit"
            | _ -> "auto"
        let str =
            match flags &&& TypeAttributes.StringFormatMask with
            | TypeAttributes.UnicodeClass -> " unicode"
            | TypeAttributes.AutoClass -> " autochar"
            | TypeAttributes.CustomFormatClass -> String.Empty
            | _ -> " ansi"
        let name = this.GetFullName()

        // TODO: Add other flags when printing TypeDefRow.
        let extends =
            match extends with
            | Extends.ConcreteClass(SimpleIndex tdef)
            | Extends.AbstractClass(SimpleIndex tdef) -> tdef.Value.GetFullName() |> sprintf " extends %s"
            | Extends.TypeRef tref -> sprintf " extends %O" tref
            | Extends.Null -> String.Empty

        sprintf ".class %s %s%s %s%s" visibility layout str name extends

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
/// Represents a <c>TypeDef</c> that is neither a delegate, enumeration, interface, or user-defined value type.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.ConcreteClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.AbstractClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.SealedClassDef"/>
/// <seealso cref="T:FSharpIL.Metadata.StaticClassDef"/>
type ClassDef<'Flags> =
    { /// <summary>
      /// Corresponds to the <c>VisibilityMask</c> flags of a type, as well as an entry in the <c>NestedClass</c> table if the current type is nested.
      /// </summary>
      Access: TypeVisibility
      Flags: ValidFlags<'Flags, TypeAttributes>
      ClassName: Identifier
      TypeNamespace: string
      Extends: Extends }

/// Represents a class that is not sealed or abstract.
type ConcreteClassDef = ClassDef<ConcreteClassTag>
/// Represents an abstract class.
type AbstractClassDef = ClassDef<AbstractClassTag>
type SealedClassDef = ClassDef<SealedClassTag>
// TODO: Remove Extends field for static classes, and make them inherit from System.Object if this is a requirement by ECMA-335.
/// Represents a sealed and abstract class, meaning that it can only contain static members.
type StaticClassDef = ClassDef<StaticClassTag>

/// <summary>
/// Represents a delegate type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Delegate"/>.
/// </summary>
type DelegateDef =
    { Access: TypeVisibility
      Flags: DelegateFlags
      DelegateName: Identifier
      TypeNamespace: string }

/// <summary>
/// Represents an enumeration type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Enum"/>.
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
/// Represents a user-defined value type, which is a <c>TypeDef</c> that derives from <see cref="T:System.ValueType"/>.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.EnumDef"/>
type StructDef =
   { Access: TypeVisibility
     Flags: TypeFlags<StructFlags>
     StructName: Identifier
     TypeNamespace: string }

/// <summary>Represents a violation of CLS rule 19, which states that "CLS-compliant interfaces shall not define...fields".</summary>
/// <category>CLS Rules</category>
[<Sealed>]
type InterfaceContainsFields (intf: InterfaceDef) =
    // TODO: Mention name of offending interface when it contains fields.
    inherit ClsViolation({ Number = 19uy; Message = sprintf "Interfaces should not define fields" })
    member _.Interface = intf

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

/// <summary>
/// Error used when a system type such as <see cref="T:System.ValueType"/> or <see cref="T:System.Delegate"/> could not be found.
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type MissingTypeError (ns: string, name: Identifier) =
    inherit ValidationError()
    member _.Namespace = ns
    member _.Name = name
    override _.ToString() =
        match ns with
        | "" -> string name
        | _ -> sprintf "%s.%A" ns name
        |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"

/// <summary>Represents the <c>&lt;Module&gt;</c> pseudo-class that contains global fields and methods (II.10.8).</summary>
[<AbstractClass; Sealed>]
type ModuleType private () =
    static member val Name = Identifier.ofStr "<Module>"
    static member val internal Row = TypeDefRow(TypeAttributes.NotPublic, ModuleType.Name, String.Empty, Extends.Null, None)

[<Sealed>]
type TypeDefTableBuilder internal (owner: IndexOwner) =
    let definitions = MetadataTableBuilder<TypeDefRow> owner

    member val Module =
        match definitions.TryAdd ModuleType.Row with
        | ValueSome i -> TypeDefIndex<ModuleType> i
        | ValueNone -> failwith "Unable to add <Module> type to TypeDef table"

    member _.Count = definitions.Count

    member _.GetEnumerator() = definitions.GetEnumerator()

    // TODO: Add methods for adding classes, structs, etc. that are called by the functions in the CliMetadata module.
    member internal _.TryAdd(typeDef: TypeDefRow) = definitions.TryAdd typeDef

    member internal _.ToImmutable() = definitions.ToImmutable()

    interface IReadOnlyCollection<TypeDefRow> with
        member this.Count = this.Count
        member _.GetEnumerator() = (definitions :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (definitions :> System.Collections.IEnumerable).GetEnumerator()
