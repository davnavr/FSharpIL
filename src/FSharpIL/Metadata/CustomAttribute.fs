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

[<RequireQualifiedAccess>]
type CustomAttributeParent = // TODO: Make custom attribute parent a struct
    | MethodDef of RawIndex<MethodDefRow>
    // | Field // of ?
    // | TypeRef // of ?
    | TypeDef of RawIndex<TypeDefRow>
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
    | Assembly of RawIndex<Assembly>
    // | AssemblyRef // of ?
    | File of RawIndex<File>
    // | ExportedType // of ?
    // | ManifestResource // of ?
    // | GenericParam // of ?
    // | GenericParamConstraint // of ?
    // | MethodSpec // of ?

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
      Value: CustomAttributeSignature option }
      // TODO: How to validate signature to ensure types of fixed arguments match method signature? Maybe have FixedArgs field of signature type be ParamItem -> int -> FixedArg?

[<Sealed>]
type CustomAttributeTable internal () =
    let attrs = List<CustomAttribute>()

    member _.Count = attrs.Count

    member _.Add(attr: CustomAttribute) = attrs.Add attr

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator
