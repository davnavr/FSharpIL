namespace rec FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Printf

/// II.23.2.8
type TypeDefOrRefOrSpecEncoded =
    | TypeDef of SimpleIndex<TypeDefRow>
    | TypeRef of SimpleIndex<TypeRef>
    // TypeSpec // of ?

    override this.ToString() =
        match this with
        | TypeDef tdef -> tdef.Value.ToString()
        | TypeRef tref -> tref.Value.ToString()

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

[<RequireQualifiedAccess>]
type GenericInst =
    // TODO: Add other GenericInst types for TypeDefs.
    | TypeRef of valueType: bool * SimpleIndex<TypeRef> * head: EncodedType * tail: ImmutableArray<EncodedType>

    override this.ToString() =
        match this with
        | TypeRef(_, tref, head, tail) ->
            let tail' =
                let text = StringBuilder()
                for gparam in tail do
                    bprintf text ", %O" gparam
                text.ToString()
            sprintf "%O<%O%O>" tref.Value head tail'

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | TypeRef(_, item, head, tail) ->
                IndexOwner.checkIndex owner item
                IndexOwner.checkOwner owner head
                for gparam in tail do IndexOwner.checkOwner owner gparam

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
    | GenericInst of GenericInst
    /// Represents a generic parameter in a generic method.
    | MVar of number: uint32
    /// <summary>Represents the <see cref="T:System.Object"/> type.</summary>
    | Object
    //| PTR // of ?
    //| PTR // of ?
    /// <summary>Represents the <see cref="T:System.String"/> type.</summary>
    | String
    | SZArray of ImmutableArray<CustomModifier> * EncodedType
    | ValueType of TypeDefOrRefOrSpecEncoded
    /// Represents a generic parameter in a generic type definition.
    | Var of number: uint32

    override this.ToString() =
        match this with
        | U4 -> "uint32"
        | GenericInst inst -> string inst
        | MVar num -> sprintf "!!%i" num
        | String -> "string"
        | SZArray(_, item) -> sprintf "%O[]" item
        | ValueType item -> sprintf "valuetype %O" item
        | Var num -> sprintf "!%i" num
        | _ -> "unknown encoded type"

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
            | MVar _
            | String
            | Var _ -> ()
            | Array(item, _) -> IndexOwner.checkOwner owner item
            | GenericInst inst -> IndexOwner.checkOwner owner inst
            | SZArray(modifiers, item) ->
                for modifier in modifiers do modifier.CheckOwner owner
                IndexOwner.checkOwner owner item
            | ValueType item -> IndexOwner.checkOwner owner item
            | bad -> failwithf "Cannot validate owner of unsupported encoded type %A" bad

/// <summary>Represents all different possible return types encoded in a <c>RetType</c> (II.23.2.11).</summary>
/// <seealso cref="T:FSharpIL.Metadata.ReturnTypeItem"/>
[<RequireQualifiedAccess>]
type ReturnType =
    | Type of EncodedType
    | ByRefType of EncodedType
    | TypedByRef
    | Void

    override this.ToString() =
        match this with
        | Type t -> string t
        | Void -> "void"
        | _ -> "unknown return type"

    interface IReturnType
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | Type item
            | ByRefType item -> IndexOwner.checkOwner owner item
            | TypedByRef
            | Void -> ()

/// <summary>Represents a <c>TypeSpec</c> item in the <c>#Blob</c> heap (II.23.2.14).</summary>
[<RequireQualifiedAccess>]
type TypeSpec =
    /// <summary>Represents a <c>GENERICINST</c> followed by a <c>TypeRef</c>.</summary>
    | GenericInst of GenericInst

    override this.ToString() =
        match this with
        | GenericInst inst -> inst.ToString()

    interface ITypeSpec
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | GenericInst generic -> IndexOwner.checkOwner owner generic

/// <summary>Represents a <c>MethodSpec</c> item in the <c>#Blob</c> heap (II.23.2.15).</summary>
/// <exception cref="T:System.ArgumentException">Thrown when the generic argument list is empty.</exception>
type MethodSpec (garguments: ImmutableArray<EncodedType>) =
    do if garguments.Length <= 0 then invalidArg "garguments" "The generic argument list cannot be empty."

    new (garguments: seq<_>) = MethodSpec(garguments.ToImmutableArray())

    member _.Count = garguments.Length

    override this.Equals obj = (this :> IEquatable<MethodSpec>).Equals(obj :?> MethodSpec)
    override _.GetHashCode() = garguments.GetHashCode()

    member _.ToImmutableArray() = garguments

    interface IEquatable<MethodSpec> with
        member this.Equals other = this.ToImmutableArray() = other.ToImmutableArray()

    interface IMethodSpec
    interface IIndexValue with
        member _.CheckOwner owner = for gparam in garguments do IndexOwner.checkOwner owner gparam

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EncodedType =
    let typeDefStruct (typeDef: TypeDefIndex<_>) = TypeDefOrRefOrSpecEncoded.TypeDef typeDef.Index |> EncodedType.ValueType
    let typeRefStruct (typeRef: SimpleIndex<TypeRef>) = TypeDefOrRefOrSpecEncoded.TypeRef typeRef |> EncodedType.ValueType

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReturnType =
    let item (returnType: ReturnType) = ReturnTypeItem returnType
    let encoded encodedType = ReturnTypeItem(ReturnType.Type encodedType)
    let modified modifiers (returnType: ReturnType) = ReturnTypeItem(modifiers, returnType)
    let itemVoid = ReturnTypeItem ReturnType.Void
    let itemBool = encoded EncodedType.Boolean
    let itemI4 = encoded EncodedType.I4
    let itemU4 = encoded EncodedType.U4
    let itemVar number = EncodedType.Var number |> encoded

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GenericInst =
    // TODO: Figure out if using list when making generic TypeSpec is bad for performance.
    /// <exception cref="T:System.ArgumentException">Thrown when the generic parameter list is empty.</exception>
    let typeRef valueType typeRef genericParameters =
        match genericParameters with
        | head :: tail -> GenericInst.TypeRef(valueType, typeRef, head, tail.ToImmutableArray())
        | _ -> invalidArg "genericParameters" "The generic parameter list should not be empty."

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeSpec =
    let create (typeSpec: TypeSpec) = TypeSpecRow typeSpec
    let genericInst (inst: GenericInst) = TypeSpec.GenericInst inst |> create

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParamItem =
    let modified modifiers (paramType: EncodedType) = ParamItem(modifiers, paramType)
    let create paramType = modified ImmutableArray.Empty paramType
    let mvar num = EncodedType.MVar num |> create
    let var num = EncodedType.Var num |> create

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FieldSignature =
    let modified modifiers (fieldType: EncodedType) = FieldSignature(modifiers, fieldType)
    let create fieldType = modified ImmutableArray.Empty fieldType
