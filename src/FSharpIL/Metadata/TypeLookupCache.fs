namespace FSharpIL.Metadata

open System.Collections.Generic

[<StructuralComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnly>]
type private TypeLookupKey = struct
    val TypeNamespace: string
    val TypeName: Identifier
    new (typeNamespace, typeName) = { TypeNamespace = typeNamespace; TypeName = typeName }
end

[<RequireQualifiedAccess>]
type TypeLookupResult =
    | TypeDef of SimpleIndex<TypeDefRow>
    | TypeRef of SimpleIndex<TypeRef>

[<Sealed>]
type TypeLookupCache (builder: CliMetadataBuilder) =
    let cache = Dictionary<TypeLookupKey, TypeLookupResult>()

    member private _.CreateIndex value = SimpleIndex<_>(builder.Owner, value)

    member this.FindType(typeNamespace, typeName) =
        let key = TypeLookupKey(typeNamespace, typeName)
        match cache.TryGetValue key with
        | (true, existing) -> ValueSome existing
        | (false, _) ->
            let mutable result = ValueNone

            let mutable typeRefEnumerator = builder.TypeRef.GetEnumerator()
            while result.IsNone && typeRefEnumerator.MoveNext() do
                let tref = typeRefEnumerator.Current
                if tref.TypeName = typeName && tref.TypeNamespace = typeNamespace then
                    let index = this.CreateIndex tref
                    result <- ValueSome (TypeLookupResult.TypeRef index)

            let mutable typeDefEnumerator = builder.TypeDef.GetEnumerator()
            while result.IsNone && typeDefEnumerator.MoveNext() do
                let tdef = typeDefEnumerator.Current
                if tdef.TypeName = typeName && tdef.TypeNamespace = typeNamespace then
                    let index = this.CreateIndex tdef
                    result <- ValueSome (TypeLookupResult.TypeDef index)

            result
