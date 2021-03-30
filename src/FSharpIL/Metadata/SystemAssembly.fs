namespace FSharpIL.Metadata.SystemAssembly

open System

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Checked

/// Contains functions to reference commonly used .NET 5 assemblies.
module Net5_0 =
    /// <summary>
    /// Adds a reference to the assembly that contains core system types such as <see cref="T:System.Object"/>.
    /// </summary>
    let private_corelib (builder: CliMetadataBuilder) =
        let token = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
        let token' = builder.Blobs.MiscBytes.TryAdd token |> Result.any
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyOrToken token'
          Name = AssemblyName.ofStr "System.Private.CoreLib"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly builder

    /// <summary>Adds a reference to the assembly containing the <see cref="T:System.Console"/> type.</summary>
    let console (builder: CliMetadataBuilder) =
        let token = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
        let token' = builder.Blobs.MiscBytes.TryAdd token |> Result.any
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyOrToken token'
          Name = AssemblyName.ofStr "System.Console"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly builder
