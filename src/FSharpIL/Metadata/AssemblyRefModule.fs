[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.AssemblyRef

open System.Collections.Immutable
open System.Reflection

let inline addRow (builder: CliMetadataBuilder) (assembly: AssemblyRef) =
    let mutable dup = false
    let i = builder.AssemblyRef.Add(&assembly, &dup)
    struct(i, dup)

let inline createRow builder version name publicKeyOrToken culture hashValue =
    AssemblyRef(version, name, publicKeyOrToken, culture, hashValue) |> addRow builder

let inline addReflectedRow (builder: CliMetadataBuilder) (assembly: Assembly) =
    let name = assembly.GetName()
    let ptoken =
        // TODO: Fix, all System.Reflection.Assembly instances retrieved with reflection might have the PublicKey flag always set.
        if name.Flags.HasFlag AssemblyNameFlags.PublicKey then
            let publicKey = name.GetPublicKey().ToImmutableArray()
            PublicKeyOrToken(builder.Blobs.MiscBytes.GetOrAdd publicKey)
        else
            let publicKeyToken = name.GetPublicKeyToken()
            match publicKeyToken with
            | [| b1; b2; b3; b4; b5; b6; b7; b8; |] ->
                PublicKeyToken(b1, b2, b3, b4, b5, b6, b7, b8)
                |> builder.Blobs.MiscBytes.GetOrAdd
                |> PublicKeyOrToken
            | null
            | _ -> invalidArg "assembly" "Invalid public key token"
    let culture =
        match name.CultureName with
        | ""
        | null -> NullCulture
        | culture -> CustomCulture(Identifier.ofStr culture)

    AssemblyRef (
        name.Version,
        AssemblyName.ofStr name.Name,
        ptoken,
        culture,
        ValueNone // TODO: Get hash value of assembly if available.
    )
    |> addRow builder

let inline addRowChecked builder assembly (warnings: WarningsBuilder) =
    let struct (i, duplicate) = addRow builder assembly
    if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
    i
