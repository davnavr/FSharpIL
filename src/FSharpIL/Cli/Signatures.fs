module FSharpIL.Cli.Signatures

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Signatures

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type TypeDefOrRefOrSpecEncoded =
    val private MatchValue: obj

    internal new (encoded) = { MatchValue = encoded }

    member this.TypeSpec =
        match this.MatchValue with
        | :? EncodedType<Type, TypeDefOrRefOrSpecEncoded> as tspec -> ValueSome tspec
        | _ -> ValueNone

    member this.TypeDefOrRef =
        match this.MatchValue with
        | :? Type as t -> ValueSome t
        | _ -> ValueNone

    member this.Equals(other: TypeDefOrRefOrSpecEncoded) = this.MatchValue.Equals other

    override this.GetHashCode() = this.MatchValue.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? TypeDefOrRefOrSpecEncoded as obj -> this.Equals obj
        | _ -> false

    interface System.IEquatable<TypeDefOrRefOrSpecEncoded> with member this.Equals other = this.Equals other

type CustomMod = CustomMod<TypeDefOrRefOrSpecEncoded>
type CustomModifiers = CustomModifiers<TypeDefOrRefOrSpecEncoded>
type EncodedType = EncodedType<Type, TypeDefOrRefOrSpecEncoded>
type FieldSig = FieldSig<Type, TypeDefOrRefOrSpecEncoded>
type ParamItem = ParamItem<Type, TypeDefOrRefOrSpecEncoded>
type ReturnType = ReturnType<Type, TypeDefOrRefOrSpecEncoded>
type MethodDefSig = MethodDefSig<Type, TypeDefOrRefOrSpecEncoded>
type MethodRefSig = MethodRefSig<Type, TypeDefOrRefOrSpecEncoded>
type PropertySig = PropertySig<Type, TypeDefOrRefOrSpecEncoded>
type GenericArgList = GenericArgList<Type, TypeDefOrRefOrSpecEncoded>
type GenericInst = GenericInst<Type, TypeDefOrRefOrSpecEncoded>
type Pointer = Pointer<Type, TypeDefOrRefOrSpecEncoded>
