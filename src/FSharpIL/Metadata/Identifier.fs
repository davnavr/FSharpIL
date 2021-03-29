namespace FSharpIL.Metadata

/// <summary>
/// Represents a <see cref="T:System.String"/> that cannot be <see langword="null"/>, be empty, or contain any null characters.
/// </summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type Identifier internal (identifier: string) = override _.ToString() = identifier

[<RequireQualifiedAccess>]
module Identifier =
    let inline private create nulli empty nullc valid =
        function
        | null -> nulli()
        | "" -> empty()
        | str when str.Contains '\000' -> nullc()
        | str -> Identifier str |> valid

    /// <summary>Tries to create an identifier from the specified string.</summary>
    let tryOfStr str =
        let inline none() = None
        create none none none Some str

    /// <summary>Creates an identifier from the specified string.</summary>
    /// <exception cref="T:System.ArgumentNullException">Thrown when the input string is <see langword="null"/>.</exception>
    /// <exception cref="T:System.ArgumentException">Thrown when the input string is empty or contains a null character.</exception>
    let ofStr str =
        create
            (fun() -> nullArg "str")
            (fun() -> invalidArg "str" "The string cannot be null or empty")
            (fun() -> invalidArg "str" "The string cannot contain any null characters")
            id
            str

[<AutoOpen>]
module IdentifierPatterns = let (|Identifier|) (name: Identifier) = name.ToString()
