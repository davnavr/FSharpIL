namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

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

/// <summary>Represents a <c>FixedArg</c> item, which stores the arguments for a custom attribute's constructor method (II.23.3).</summary>
[<RequireQualifiedAccess>]
type FixedArg =
    | Elem of Elem
    | SZArray of ImmutableArray<Elem>

[<RequireQualifiedAccess>]
type NamedArg =
    | Field // of FieldOrPropType * string * FixedArg
    | Property // of FieldOrPropType * string * FixedArg

/// <summary>
/// Represents a <c>CustomAttrib</c>, which stores the arguments provided to a custom attribute's constructor,
/// as well as any values assigned to its fields or properties. (II.23.3).
/// </summary>
type CustomAttributeSignature =
    { FixedArg: ImmutableArray<FixedArg>
      NamedArg: ImmutableArray<NamedArg> }

type CustomAttributeParentTag =
    | MethodDef = 0uy
    | Field = 1uy
    | TypeRef = 2uy
    | TypeDef = 3uy
    | Param = 4uy
    | InterfaceImpl = 5uy
    | MemberRef = 6uy
    | Module = 7uy
    | Permission = 8uy
    | Property = 9uy
    | Event = 10uy
    | StandAloneSig = 11uy
    | ModuleRef = 12uy
    | TypeSpec = 13uy
    | Assembly = 14uy
    | AssemblyRef = 15uy
    | File = 16uy
    | ExportedType = 17uy
    | ManifestResource = 18uy
    | GenericParam = 19uy
    | GenericParamConstraint = 20uy
    | MethodSpec = 21uy

type CustomAttributeParent = TaggedIndex<CustomAttributeParentTag>

[<RequireQualifiedAccess>]
module CustomAttributeParent =
    let MethodDef (index: RawIndex<MethodDefRow>) = index.ToTaggedIndex CustomAttributeParentTag.MethodDef
    let TypeDef (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex CustomAttributeParentTag.TypeDef
    let Assembly (index: RawIndex<Assembly>) = index.ToTaggedIndex CustomAttributeParentTag.Assembly
    let File (index: RawIndex<File>) = index.ToTaggedIndex CustomAttributeParentTag.File

type CustomAttributeTypeTag =
   | Def = 0uy
   | RefDefault = 1uy
   | RefGeneric = 2uy
   | RefVarArg = 3uy

type CustomAttributeType = TaggedIndex<CustomAttributeTypeTag>

[<RequireQualifiedAccess>]
module CustomAttributeType =
    let (|MethodDef|MethodRefDefault|MethodRefGeneric|MethodRefVarArg|) (index: CustomAttributeType) =
        match index.Tag with
        | CustomAttributeTypeTag.RefDefault -> MethodRefDefault(index.ToRawIndex<MethodRefDefault>())
        | CustomAttributeTypeTag.RefGeneric -> MethodRefGeneric(index.ToRawIndex<MethodRefGeneric>())
        | CustomAttributeTypeTag.RefVarArg -> MethodRefVarArg(index.ToRawIndex<MethodRefVarArg>())
        | CustomAttributeTypeTag.Def
        | _ -> MethodDef(index.ToRawIndex<MethodDefRow>())
    let MethodDef (index: RawIndex<MethodDefRow>) = index.ToTaggedIndex CustomAttributeTypeTag.Def
    let MethodRefDefault (index: RawIndex<MethodRefDefault>) = index.ToTaggedIndex CustomAttributeTypeTag.RefDefault
    let MethodRefGeneric (index: RawIndex<MethodRefGeneric>) = index.ToTaggedIndex CustomAttributeTypeTag.RefGeneric
    let MethodRefVarArg (index: RawIndex<MethodRefVarArg>) = index.ToTaggedIndex CustomAttributeTypeTag.RefVarArg

/// <summary>Represents a row in the <c>CustomAttribute</c> table (II.22.10).</summary>
type CustomAttribute =
    { Parent: CustomAttributeParent
      /// Specifies the constructor method used to create the custom attribute.
      Type: CustomAttributeType // TODO: How to ensure that the MethodRef points to a .ctor?
      Value: Blob<CustomAttributeSignature> voption }
      // TODO: How to validate signature to ensure types of fixed arguments match method signature? Maybe have FixedArgs field of signature type be ParamItem -> int -> FixedArg?

[<Sealed>]
type CustomAttributeTableBuilder internal () =
    let attrs = ImmutableArray.CreateBuilder<CustomAttribute>()

    member _.Count = attrs.Count

    // TODO: Return indices pointing to CustomAttribute table.
    member _.Add(attr: inref<CustomAttribute>) = attrs.Add attr

    member _.ToImmutable() = attrs.ToImmutable()

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator
