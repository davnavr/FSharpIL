namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
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
[<AbstractClass; Sealed>] type StructFlags = class end

type TypeFlags<'Tag> = ValidFlags<'Tag, TypeAttributes>

type ExtendsTag =
    | Null = 0uy
    | ConcreteClass = 1uy
    | AbstractClass = 2uy
    | TypeRef = 3uy
    | TypeSpec = 4uy

/// <summary>
/// Specifies which type is extended by a <c>TypeDef</c>.
/// </summary>
type Extends = TaggedIndex<ExtendsTag>

[<RequireQualifiedAccess>]
module Extends =
    let (|ConcreteClass|AbstractClass|TypeRef|TypeSpec|Null|) (extends: Extends) =
        match extends.Tag with
        | ExtendsTag.ConcreteClass -> ConcreteClass(extends.ToRawIndex<ConcreteClassDef>())
        | ExtendsTag.AbstractClass -> AbstractClass(extends.ToRawIndex<AbstractClassDef>())
        | ExtendsTag.TypeRef -> TypeRef(extends.ToRawIndex<TypeRef>())
        | ExtendsTag.TypeSpec -> TypeSpec(extends.ToRawIndex<TypeSpecRow>())
        | ExtendsTag.Null
        | _ -> Null

    /// Extends a class that is not sealed or abstract.
    let ConcreteClass (index: RawIndex<ConcreteClassDef>) = index.ToTaggedIndex ExtendsTag.ConcreteClass
    /// Extends an abstract class.
    let AbstractClass (index: RawIndex<AbstractClassDef>) = index.ToTaggedIndex ExtendsTag.AbstractClass
    /// Extends a type referenced in another assembly.
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex ExtendsTag.TypeRef
    let TypeSpec (index: RawIndex<TypeSpecRow>) = index.ToTaggedIndex ExtendsTag.TypeSpec
    /// <summary>
    /// Indicates that a class does not extend another class, used by <see cref="T:System.Object"/> and interfaces.
    /// </summary>
    let Null = TaggedIndex ExtendsTag.Null

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
    member _.EnclosingClass: RawIndex<TypeDefRow> voption = parent

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

        sprintf ".class %s %s%s %s" visibility layout str name

    interface IEquatable<TypeDefRow> with
        member _.Equals other = ns = other.TypeNamespace && name = other.TypeName

type TypeVisibility = TaggedIndex<TypeAttributes>

[<RequireQualifiedAccess>]
module TypeVisibility =
    let (|NotPublic|Public|Nested|) (visibility: TypeVisibility) =
        match visibility.Tag with
        | TypeAttributes.Public -> Public
        | TypeAttributes.NotPublic -> NotPublic
        | _ -> Nested(visibility.ToRawIndex<TypeDefRow>())

    let (|NotNested|NestedPublic|NestedPrivate|NestedFamily|NestedAssembly|NestedFamilyAndAssembly|NestedFamilyOrAssembly|) (visibility: TypeVisibility) =
        match visibility.Tag with
        | TypeAttributes.Public -> NotNested
        | TypeAttributes.NotPublic -> NotNested
        | nested ->
            let tindex = visibility.ToRawIndex<TypeDefRow>
            match nested with
            | TypeAttributes.NestedPublic -> NestedPublic tindex
            | TypeAttributes.NestedPrivate -> NestedPrivate tindex
            | TypeAttributes.NestedFamily -> NestedFamily tindex
            | TypeAttributes.NestedAssembly -> NestedAssembly tindex
            | TypeAttributes.NestedFamANDAssem -> NestedFamilyAndAssembly tindex
            | TypeAttributes.NestedFamORAssem -> NestedFamilyOrAssembly tindex
            | _ -> NotNested

    let NotPublic = TaggedIndex TypeAttributes.NotPublic
    let Public = TaggedIndex TypeAttributes.Public
    let NestedPublic (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedPublic
    let NestedPrivate (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedPrivate
    /// <summary>Equivalent to the C# keyword <see langword="protected"/> applied on a nested type.</summary>
    let NestedFamily (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedFamily
    /// <summary>Equivalent to the C# keyword <see langword="internal"/> applied on a nested type.</summary>
    let NestedAssembly (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedAssembly
    /// <summary>Equivalent to the C# <see langword="private protected"/> keyword.</summary>
    let NestedFamilyAndAssembly (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedFamANDAssem
    /// <summary>Equivalent to the C# <see langword="protected internal"/> keyword.</summary>
    let NestedFamilyOrAssembly (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeAttributes.NestedFamORAssem

    /// <summary>Retrieves the enclosing class of this nested class.</summary>
    /// <remarks>In the actual metadata, nested type information is stored in the NestedClass table (II.22.32).</remarks>
    let enclosingClass (visibility: TypeVisibility) =
        match visibility with
        | Nested parent -> ValueSome parent
        | _ -> ValueNone

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

// TODO: Allow addition of additional instance or static methods to delegates, and make beginInvoke and endInvoke methods optional (I.8.9.3)
/// <summary>
/// Represents a delegate type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Delegate"/> (I.8.9.3 and II.14.6).
/// </summary>
[<IsReadOnly>]
type DelegateDef = struct
    val Access: TypeVisibility
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>
    val DelegateName: Identifier
    val TypeNamespace: string
    val Flags: TypeAttributes
    val internal MethodFlags: MethodAttributes

    internal new (access, returnType, parameters, name, ns, tflags, mflags) =
        { Access = access
          ReturnType = returnType
          Parameters = parameters
          DelegateName = name
          TypeNamespace = ns
          Flags = tflags
          MethodFlags = mflags }

    new
        (
            access: TypeVisibility,
            returnType,
            parameters,
            name,
            [<Optional; DefaultParameterValue("")>] ns: string,
            [<Optional; DefaultParameterValue(false)>] serializable: bool,
            [<Optional; DefaultParameterValue(false)>] newslot: bool,
            [<Optional; DefaultParameterValue(false)>] strict: bool
        ) =
        let mutable tflags = TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Sealed ||| access.Tag
        if serializable then tflags <- tflags ||| TypeAttributes.Serializable
        let mutable mflags = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        if newslot then mflags <- mflags ||| MethodAttributes.NewSlot
        if strict then mflags <- mflags ||| MethodAttributes.CheckAccessOnOverride
        DelegateDef(access, returnType, parameters, name, ns, tflags, mflags)
end

// TODO: Figure out how to use Blob`1 here.
[<IsReadOnly; Struct>]
type EnumValue internal (name: Identifier, value: IntegerConstantBlob) =
    member _.Name = name
    member _.Value = value

[<IsReadOnly; Struct>]
type EnumValueList internal (utype: IntegerType, values: ImmutableArray<EnumValue>) =
    member _.Count = values.Length
    member _.UnderlyingType = utype
    member _.GetEnumerator() = values.GetEnumerator()
    member _.ToImmutableArray() = values

/// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when the underlying type of the enum is invalid.</exception>
[<Sealed>]
type EnumValueListBuilder (utype: IntegerType, capacity: int32) =
    do if utype < IntegerType.Bool || utype > IntegerType.U8 then
        let msg = "A valid numeric type must be specifed for the underlying type of an enumeration"
        ArgumentOutOfRangeException("utype", utype, msg) |> raise

    let lookup = Dictionary<Identifier, IntegerConstantBlob> capacity

    new (utype) = EnumValueListBuilder(utype, 1)

    member _.UnderlyingType = utype

    member _.TryAdd(name, value: IntegerConstantBlob) =
        value.Tag = utype && lookup.TryAdd(name, value)

    member _.ToImmutable() =
        let values = ImmutableArray.CreateBuilder<EnumValue> lookup.Count
        for KeyValue(name, value) in lookup do values.Add(EnumValue(name, value))
        EnumValueList(utype, values.ToImmutable())

// TODO: Figure out how to avoid addition of methods to Enum type. Maybe prevent conversion of EnumDef index to TypeDefRow index?
/// <summary>
/// Represents an enumeration type, which is a <c>TypeDef</c> that derives from <see cref="T:System.Enum"/> (II.14.3).
/// </summary>
[<IsReadOnly>]
type EnumDef = struct
    val Access: TypeVisibility
    val EnumName: Identifier
    val TypeNamespace: string
    val Values: EnumValueList
    val Flags: TypeAttributes

    internal new (access, values, flags, name, ns) =
        { Access = access
          Flags = flags
          EnumName = name
          TypeNamespace = ns
          Values = values }

    new
        (
            access: TypeVisibility,
            values,
            name,
            [<Optional; DefaultParameterValue("")>] ns: string,
            [<Optional; DefaultParameterValue(true)>] serializable: bool
        ) =
        let mutable flags = access.Tag ||| TypeAttributes.Sealed ||| TypeAttributes.AutoLayout
        if serializable then flags <- flags ||| TypeAttributes.Serializable
        EnumDef(access, values, flags, name, ns)
end

// TODO: Prevent interfaces from having instance fields.
/// <summary>
/// Represents an interface type, which is a <c>TypeDef</c> that is abstract and does not derive from any other class (II.12).
/// </summary>
[<IsReadOnly; Struct>]
type InterfaceDef (access: TypeVisibility, name: Identifier, [<Optional; DefaultParameterValue("")>] ns: string) =
    member _.Access = access
    member _.InterfaceName = name
    member _.TypeNamespace = ns

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
    static member val internal Row = TypeDefRow(TypeAttributes.NotPublic, ModuleType.Name, String.Empty, Extends.Null, ValueNone)

[<Sealed>]
type TypeDefTableBuilder internal () =
    let definitions = RowHashSet<TypeDefRow>.Create()

    member val Module =
        match definitions.TryAdd ModuleType.Row with
        | ValueSome i -> RawIndex<ModuleType> i.Value
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
