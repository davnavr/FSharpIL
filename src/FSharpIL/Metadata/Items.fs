namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

/// II.23.2.8
type TypeDefOrRefOrSpecEncoded =
    | TypeDef of SimpleIndex<TypeDefRow>
    | TypeRef of SimpleIndex<TypeRef>
    // TypeSpec // of ?

    interface ITypeDefOrRefOrSpec
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | TypeDef tdef -> IndexOwner.checkIndex owner tdef
            | TypeRef tref -> IndexOwner.checkIndex owner tref

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CustomModifier =
    let optional (modifierType: TypeDefOrRefOrSpecEncoded) = CustomModifier(false, modifierType)
    let required (modifierType: TypeDefOrRefOrSpecEncoded) = CustomModifier(true, modifierType)

/// <summary>Represents all different possible return types encoded in a <c>RetType</c> (II.23.2.11).</summary>
/// <seealso cref="T:FSharpIL.Metadata.ReturnTypeItem"/>
[<RequireQualifiedAccess>]
type ReturnType =
    // | Type of byRef: bool * EncodedType
    // | TypedByRef // of ?
    | Void

    interface IReturnType
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | Void -> ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReturnType =
    let voidItem = ReturnTypeItem ReturnType.Void
    let create (returnType: ReturnType) = ReturnTypeItem returnType
    let modified modifiers (returnType: ReturnType) = ReturnTypeItem(modifiers, returnType)

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

    interface IEncodedType
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | Boolean
            | Char
            | I1
            | U1
            | I2
            | U2
            | I4
            | U4
            | I8
            | U8
            | R4
            | R8
            | I
            | U
            | Object
            | String -> ()
            | Array(item, _) -> IndexOwner.checkOwner owner item
            | _ -> failwith "Cannot validate owner of unsupported encoded type"

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParamItem =
    let modified modifiers (paramType: EncodedType) = ParamItem(modifiers, paramType)
    let inline create paramType = modified ImmutableArray.Empty paramType
