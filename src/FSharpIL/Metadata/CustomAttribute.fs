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

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | Field
            | Property -> ()

/// <summary>
/// Represents a <c>CustomAttrib</c>, which stores the arguments provided to a custom attribute's constructor,
/// as well as any values assigned to its fields or properties. (II.23.3).
/// </summary>
type CustomAttributeSignature =
    { FixedArg: ImmutableArray<FixedArg>
      NamedArg: ImmutableArray<NamedArg> }

    interface IIndexValue with
        member this.CheckOwner owner =
            for namedArg in this.NamedArg do
                IndexOwner.checkOwner owner namedArg

[<RequireQualifiedAccess>]
type CustomAttributeParent =
    // | MethodDef // of ?
    // | Field // of ?
    // | TypeRef // of ?
    | TypeDef of SimpleIndex<TypeDefRow>
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
      Value: CustomAttributeSignature option }
      // TODO: How to validate signature to ensure types of fixed arguments match method signature? Maybe have FixedArgs field of signature type be ParamItem -> int -> FixedArg?

    interface IIndexValue with
        member this.CheckOwner owner =
            match this.Parent with
            | CustomAttributeParent.TypeDef tdef -> IndexOwner.checkIndex owner tdef
            | CustomAttributeParent.Assembly(IndexOwner other) -> IndexOwner.ensureEqual owner other

            match this.Type with
            | CustomAttributeType.MemberRef mref -> IndexOwner.checkIndex owner mref.Index

            Option.iter (IndexOwner.checkOwner owner) this.Value

[<Sealed>]
type CustomAttributeTable internal (owner: IndexOwner) =
    let attrs = List<CustomAttribute>()

    member _.Count = attrs.Count

    member _.Add(attr: CustomAttribute) =
        IndexOwner.checkOwner owner attr
        attrs.Add attr

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator
