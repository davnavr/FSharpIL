module FSharpIL.Metadata.Signatures.MetadataSignatures

open FSharpIL.Metadata.Tables

/// <summary>Represents an index into the <c>TypeDef</c> or <c>TypeRef</c> table (II.23.2.8).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeDefOrRefEncoded internal (tag: bool, index: uint32) =
    member _.IsDefinition = tag
    member _.Index = index

[<RequireQualifiedAccess>]
module TypeDefOrRefEncoded =
    let toCodedIndex (index: TypeDefOrRefEncoded) =
        if index.IsDefinition
        then TypeDefOrRef.Def { TableIndex = index.Index }
        else TypeDefOrRef.Ref { TableIndex = index.Index }

    let Def (index: TableIndex<TypeDefRow>) = TypeDefOrRefEncoded(true, index.TableIndex)
    let Ref (index: TableIndex<TypeRefRow>) = TypeDefOrRefEncoded(false, index.TableIndex)

/// <summary>Represents an index into the <c>TypeDef</c>, <c>TypeRef</c> or <c>TypeSpec</c> table (II.23.2.8).</summary>
type TypeDefOrRefOrSpecEncoded = TypeDefOrRef

type CustomMod = CustomMod<TypeDefOrRefOrSpecEncoded>
type CustomModifiers = CustomModifiers<TypeDefOrRefOrSpecEncoded>
type EncodedType = EncodedType<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type FieldSig = FieldSig<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type ParamItem = ParamItem<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type ReturnType = ReturnType<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type MethodDefSig = MethodDefSig<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type MethodRefSig = MethodRefSig<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type PropertySig = PropertySig<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type GenericArgList = GenericArgList<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type GenericInst = GenericInst<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
type Pointer = Pointer<TypeDefOrRefEncoded, TypeDefOrRefOrSpecEncoded>
