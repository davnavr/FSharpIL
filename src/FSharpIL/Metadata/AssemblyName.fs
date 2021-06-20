namespace FSharpIL.Metadata

/// Represents the name of an assembly (II.22.2).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type AssemblyName = // TODO: Make sure Assembly and AssemblyRef use a special offset type.
    internal { AssemblyName: Identifier }
    override this.ToString() = this.AssemblyName.ToString()

[<RequireQualifiedAccess>]
module AssemblyName =
    let tryOfStr (str: string) =
        match Identifier.tryOfStr str with
        | ValueSome name when str.IndexOfAny [| ':'; '\\'; '/' |] = -1 -> ValueSome { AssemblyName = name }
        | ValueSome _
        | ValueNone -> ValueNone

    /// <summary>Creates a name for an <c>Assembly</c> or <c>AssemblyRef</c>.</summary>
    /// <exception cref="T:System.ArgumentException">
    /// The name is empty or contains a colon, a forward slash, or a backslash character.
    /// </exception>
    let ofStr (str: string) =
        match tryOfStr str with
        | ValueSome name -> name
        | ValueNone -> invalidArg "str" "The assembly name was empty or contains invalid characters."
