namespace FSharpIL.Metadata

open System

/// Represents the name of an assembly (II.22.2).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type AssemblyName internal (name: string) =
    override _.ToString() = name

/// (II.23.1.3)
type AssemblyCulture = // TODO: Add more cultures
    | NullCulture
    | CustomCulture of Identifier
    | Ar_SA
    | En_US
    | Div_MV

    override this.ToString() =
        match this with
        | NullCulture -> ""
        | Ar_SA -> "ar-SA"
        | En_US -> "en-US"
        | Div_MV -> "div-MV"
        | CustomCulture name -> name.ToString()

/// (II.23.1.1)
type HashAlgorithmId =
    | None = 0u
    | MD5 = 0x8003u
    | SHA1 = 0x8004u

/// <summary>(0x20) Represents the optional row in the <c>Assembly</c> table (II.22.2)</summary>.
type Assembly =
    { HashAlgId: unit
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

[<RequireQualifiedAccess>]
module AssemblyName =
    let tryOfStr (str: string) =
        if String.IsNullOrEmpty str || str.IndexOfAny [| ':'; '\\'; '/' |] > -1
        then None
        else AssemblyName str |> Some

    /// <exception cref="T:System.ArgumentException">
    /// The name is empty or contains a colon, a forward slash, or a backslash character.
    /// </exception>
    let ofStr (str: string) =
        match tryOfStr str with
        | Some name -> name
        | None -> invalidArg "str" "The assembly name was empty or contains invalid characters."

[<AutoOpen>]
module AssemblyNamePatterns = let (|AssemblyName|) (name: AssemblyName) = name.ToString()
