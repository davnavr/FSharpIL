/// <summary>Contains functions to reference commonly used types in the <c>System</c> namespace.</summary>
module FSharpIL.Metadata.SystemType

open FSharpIL.Metadata.Unchecked

let inline private create builder assembly name =
    { TypeName = Identifier.ofStr name
      TypeNamespace = "System"
      ResolutionScope = ResolutionScope.AssemblyRef assembly }
    |> referenceType builder

/// <summary>Adds a reference to the <see cref="T:System.Object"/> type.</summary>
/// <param name="builder"/>
/// <param name="mscorlib">
/// The assembly containing the <see cref="T:System.Object"/> type, which is <c>System.Private.CoreLib</c> for .NET Core.
/// </param>
let object builder mscorlib = create builder mscorlib "Object"

let sdelegate builder mscorlib = create builder mscorlib "delegate"
let enum builder mscorlib = create builder mscorlib "Enum"

/// <summary>Adds a reference to the <see cref="T:System.ValueType"/> type.</summary>
let valueType builder mscorlib = create builder mscorlib "ValueType"

/// <summary>Adds a reference to the <see cref="T:System.Console"/> type.</summary>
/// <param name="builder"/>
/// <param name="consolelib">
/// The assembly containing the <see cref="T:System.Console"/> type, which is <c>System.Console</c> for .NET Core.
/// </param>
let console builder consolelib = create builder consolelib "Console"
