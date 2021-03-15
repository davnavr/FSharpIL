/// <summary>Contains functions to reference commonly used types in the <c>System</c> namespace.</summary>
module FSharpIL.Metadata.SystemTypes

open FSharpIL.Metadata.CliMetadata

/// <summary>Adds a reference to the <see cref="T:System.Object"/> type.</summary>
/// <param name="mscorlib">
/// The assembly containing the <see cref="T:System.Object"/> type, which is <c>System.Private.CoreLib</c> for .NET Core.
/// </param>
let object builder mscorlib =
    { TypeName = Identifier.ofStr "Object"
      TypeNamespace = "System"
      ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
    |> referenceType builder

/// <summary>Adds a reference to the <see cref="T:System.ValueType"/> type.</summary>
let valueType builder mscorlib =
    { TypeName = Identifier.ofStr "ValueType"
      TypeNamespace = "System"
      ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
    |> referenceType builder

/// <summary>Adds a reference to the <see cref="T:System.Console"/> type.</summary>
/// <param name="mscorlib">
/// The assembly containing the <see cref="T:System.Console"/> type, which is <c>System.Console</c> for .NET Core.
/// </param>
let console builder consolelib =
    { TypeName = Identifier.ofStr "Console"
      TypeNamespace = "System"
      ResolutionScope = ResolutionScope.AssemblyRef consolelib }
    |> referenceType builder
