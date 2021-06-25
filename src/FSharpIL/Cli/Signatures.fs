﻿module FSharpIL.Cli.Signatures

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Signatures

// TODO: Use anonymous type-tagged unions when available.

let inline private (|MatchValue|) encoded = (^T : (member MatchValue : obj) encoded)

let inline private encodedTypeEquality (MatchValue current: ^T) (obj: obj) =
    match obj with
    | :? 'T as other ->
        match current, (|MatchValue|) other with
        | (:? DefinedType as x), (:? DefinedType as y) -> x = y
        | (:? ReferencedType as x), (:? ReferencedType as y) -> x = y
        //| (:? TypeSpec as x), (:? TypeSpec as y) -> x = y
        | _, _ -> false
    | _ -> false

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type TypeDefOrRefEncoded internal (encoded: obj) =
    member _.MatchValue = encoded
    override _.GetHashCode() = encoded.GetHashCode()
    override this.Equals obj = encodedTypeEquality this obj

[<RequireQualifiedAccess>]
module TypeDefOrRefEncoded =
    let inline (|Def|Ref|) (encoded: TypeDefOrRefEncoded) =
        match encoded.MatchValue with
        | :? DefinedType as tdef -> Def tdef
        | :? ReferencedType as tref -> Ref tref
        | _ ->
            encoded.MatchValue.GetType().Name
            |> sprintf "Invalid defined or referenced type %s"
            |> invalidArg "encoded"

    let Def (tdef: DefinedType) = TypeDefOrRefEncoded tdef
    let Ref (tref: ReferencedType) = TypeDefOrRefEncoded tref

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type TypeDefOrRefOrSpecEncoded internal (encoded: obj) =
    member _.MatchValue = encoded
    override _.GetHashCode() = encoded.GetHashCode()
    override this.Equals obj = encodedTypeEquality this obj

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
