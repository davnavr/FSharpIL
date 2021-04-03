namespace FSharpIL.Metadata.SystemAssembly

open System

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Checked

/// Contains functions to reference commonly used .NET 5 assemblies.
[<Obsolete("Don't reference assemblies with built in functions", true)>]
module Net5_0 =
    /// <summary>
    /// Adds a reference to the assembly that contains core system types such as <see cref="T:System.Object"/>.
    /// </summary>
    let private_corelib (builder: CliMetadataBuilder) =
        failwith "Unused"

    /// <summary>Adds a reference to the assembly containing the <see cref="T:System.Console"/> type.</summary>
    let console (builder: CliMetadataBuilder) =
        failwith "Unused"
