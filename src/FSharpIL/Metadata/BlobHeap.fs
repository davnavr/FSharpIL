namespace FSharpIL.Metadata

open System.Collections.Immutable

open FSharpIL

/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type BlobHeap internal
    (
        fieldSig: BlobLookup<FieldSignature>,
        methodDefSig: MethodDefSigBlobLookup,
        methodRefSig: MethodRefSigBlobLookup,
        constant: ConstantBlobLookup,
        customAttribute: BlobLookup<CustomAttributeSignature>,
        propertySig: PropertySigBlobLookup,
        typeSpec: TypeSpecBlobLookup,
        methodSpec: MethodSpecBlobLookup,
        localVarSig: BlobLookup<MethodLocalVariables>,
        miscBytes: BlobLookup<ImmutableArray<byte>>
    ) =
    member _.FieldSig = fieldSig
    member _.MethodDefSig = methodDefSig
    member _.MethodRefSig = methodRefSig
    member _.Constant = constant
    member _.CustomAttribue = customAttribute

    member _.PropertySig = propertySig

    member _.TypeSpec = typeSpec

    member _.MethodSpec = methodSpec

    member _.LocalVarSig = localVarSig
    /// Contains miscellaneous blobs, such as the hash values of files or the public keys of assemblies.
    member _.MiscBytes = miscBytes
    member _.SignatureCount =
        fieldSig.Count
        + methodDefSig.Count
        + methodRefSig.Count
        + constant.Count
        + customAttribute.Count

        + propertySig.Count

        + typeSpec.Count

        + methodSpec.Count

        + localVarSig.Count

        + miscBytes.Count

[<Sealed>]
type MiscBlobLookupBuilder internal () =
    let lookup = BlobLookupBuilder<ImmutableArray<byte>>()

    member _.Count = lookup.Count
    member _.TryAdd(bytes: ImmutableArray<byte>) = lookup.TryAdd bytes

    member this.TryAdd(publicKeyToken: PublicKeyToken) =
        let bytes = publicKeyToken.ToArray().ToImmutableArray()
        match this.TryAdd bytes with
        | Ok i -> Ok(i.ChangeTag<PublicKeyToken>())
        | Error err -> Error(err.ChangeTag<PublicKeyToken>())

    member this.GetOrAdd(publicKeyToken: PublicKeyToken) = this.TryAdd publicKeyToken |> Result.any

    member internal _.ToImmutable() = lookup.ToImmutable()

[<Sealed>]
type BlobHeapBuilder internal () =
    member val FieldSig = BlobLookupBuilder<FieldSignature>()
    member val MethodDefSig = MethodDefSigBlobLookupBuilder()
    member val MethodRefSig = MethodRefSigBlobLookupBuilder()
    member val Constant = ConstantBlobLookupBuilder()
    member val CustomAttribute = BlobLookupBuilder<CustomAttributeSignature>()

    member val PropertySig = PropertySigBlobLookupBuilder()

    member val TypeSpec = TypeSpecBlobLookupBuilder()

    member val MethodSpec = MethodSpecBlobLookupBuilder()

    member val LocalVarSig = BlobLookupBuilder<MethodLocalVariables>()
    member val MiscBytes = MiscBlobLookupBuilder()
    member internal this.ToImmutable() =
        BlobHeap (
            this.FieldSig.ToImmutable(),
            this.MethodDefSig.ToImmutable(),
            this.MethodRefSig.ToImmutable(),
            this.Constant.ToImmutable(),
            this.CustomAttribute.ToImmutable(),

            this.PropertySig.ToImmutable(),

            this.TypeSpec.ToImmutable(),

            this.MethodSpec.ToImmutable(),
            this.LocalVarSig.ToImmutable(),
            this.MiscBytes.ToImmutable()
        )
