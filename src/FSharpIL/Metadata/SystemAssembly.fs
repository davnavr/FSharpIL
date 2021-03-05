namespace FSharpIL.Metadata.SystemAssembly

open System

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata

/// Contains functions to reference commonly used .NET 5 assemblies.
module Net5_0 =
    /// <summary>
    /// Adds a reference to the assembly that contains core system types such as <see cref="T:System.Object"/>.
    /// </summary>
    let private_corelib =
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
          Name = AssemblyName.ofStr "System.Private.CoreLib"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly

    /// <summary>Adds a reference to the assembly containing the <see cref="T:System.Console"/> type.</summary>
    let console =
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
          Name = AssemblyName.ofStr "System.Console"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly
