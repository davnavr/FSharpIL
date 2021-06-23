namespace FSharpIL.Metadata

open System
open System.Runtime.CompilerServices

/// <summary>Represents an offset into the <c>#Strings</c> metadata stream pointing to a non-empty string.</summary>
type [<IsReadOnly; Struct; RequireQualifiedAccess>] IdentifierOffset = internal { Offset: StringOffset }
/// <summary>Represents an offset into the <c>#Strings</c> metadata stream pointing to the name of an assembly or file.</summary>
type [<IsReadOnly; Struct; RequireQualifiedAccess>] FileNameOffset = internal { Offset: IdentifierOffset }

/// <summary>
/// Represents a <see cref="T:System.String"/> that cannot be <see langword="null"/>, be empty, or contain any null characters.
/// </summary>
[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type Identifier = struct
    val private identifier: string
    internal new (identifier) = { identifier = identifier }
    member this.AsMemory() = this.identifier.AsMemory()
    override this.ToString() = this.identifier
end

/// Represents the name of an assembly (II.22.2) or file (II.22.19).
[<IsReadOnly; Struct>]
type FileName =
    internal { FileName: Identifier }
    override this.ToString() = this.FileName.ToString()

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
        let inline none() = ValueNone
        create none none none ValueSome str

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

    let inline asMemory (id: Identifier) = id.AsMemory()

[<RequireQualifiedAccess>]
module FileName =
    let tryOfStr (str: string) =
        match Identifier.tryOfStr str with
        | ValueSome name when str.IndexOfAny [| ':'; '\\'; '/' |] = -1 -> ValueSome { FileName = name }
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

[<AutoOpen>]
module StringOffsetPatterns =
    let (|IdentifierOffset|) { IdentifierOffset.Offset = offset } = offset
    let (|FileNameOffset|) { FileNameOffset.Offset = offset } = offset.Offset
