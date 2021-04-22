namespace FSharpIL.Metadata

open System.Collections.Generic

[<StructuralComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnly>]
type private TypeLookupKey = struct
    val TypeNamespace: string
    val TypeName: Identifier
    new (typeNamespace, typeName) = { TypeNamespace = typeNamespace; TypeName = typeName }
end

type TypeLookupResultTag =
    | Def = 0uy
    | Ref = 1uy

type TypeLookupResult = TaggedIndex<TypeLookupResultTag>

[<RequireQualifiedAccess>]
module TypeLookupResult =
    let (|TypeDef|TypeRef|) (result: TypeLookupResult) =
        match result.Tag with
        | TypeLookupResultTag.Ref -> TypeRef(result.ToRawIndex<TypeRef>())
        | TypeLookupResultTag.Def
        | _ -> TypeDef(result.ToRawIndex<TypeDefRow>())
    let TypeDef (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeLookupResultTag.Def
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex TypeLookupResultTag.Ref

[<Sealed>]
type TypeLookupCache (builder: CliMetadataBuilder) =
    let cache = Dictionary<TypeLookupKey, TypeLookupResult>()

    member _.TryFindType(typeNamespace, typeName) =
        let key = TypeLookupKey(typeNamespace, typeName)
        match cache.TryGetValue key with
        | (true, existing) -> ValueSome existing
        | (false, _) ->
            let mutable result, i = ValueNone, 0

            let mutable typeRefEnumerator = builder.TypeRef.GetEnumerator()
            while result.IsNone && typeRefEnumerator.MoveNext() do
                i <- i + 1
                let tref = typeRefEnumerator.Current
                if tref.TypeName = typeName && tref.TypeNamespace = typeNamespace then
                    result <- ValueSome(TypeLookupResult.TypeRef(RawIndex i))

            i <- 0
            let mutable typeDefEnumerator = builder.TypeDef.GetEnumerator()
            while result.IsNone && typeDefEnumerator.MoveNext() do
                i <- i + 1
                let tdef = typeDefEnumerator.Current
                if tdef.TypeName = typeName && tdef.TypeNamespace = typeNamespace then
                    result <- ValueSome(TypeLookupResult.TypeDef(RawIndex i))

            result

    member this.TryFindTypeEncoded(typeNamespace, typeName, isValueType) =
        match this.TryFindType(typeNamespace, typeName) with
        | ValueSome result ->
            let t =
                match result with
                | TypeLookupResult.TypeDef tdef -> TypeDefOrRefOrSpecEncoded.TypeDef tdef
                | TypeLookupResult.TypeRef tref -> TypeDefOrRefOrSpecEncoded.TypeRef tref
            if isValueType
            then EncodedType.ValueType t
            else EncodedType.Class t
            |> ValueSome
        | ValueNone -> ValueNone
