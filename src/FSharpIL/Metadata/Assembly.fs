namespace FSharpIL.Metadata

open System

/// Represents the name of an assembly (II.22.2).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type AssemblyName =
    private
    | AssemblyName of string

    override this.ToString() = let (AssemblyName name) = this in name

type AssemblyCulture = // TODO: Add more cultures
    | NullCulture
    | Ar_SA
    | En_US
    | Div_MV

    override this.ToString() =
        match this with
        | NullCulture -> ""
        | Ar_SA -> "ar-SA"
        | En_US -> "en-US"
        | Div_MV -> "div-MV"

/// <summary>(0x20) Represents the optional row in the <c>Assembly</c> table (II.22.2)</summary>.
type Assembly =
    { HashAlgId: unit
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

type AssemblyIndex = TaggedIndex<Assembly, unit>

[<RequireQualifiedAccess>]
module AssemblyName =
    let tryOfStr (str: string) =
        if String.IsNullOrEmpty str || str.IndexOfAny [| ':'; '\\'; '/' |] > -1
        then None
        else AssemblyName str |> Some

    /// <exception cref="T:System.ArgumentException">The <paramref name="name"/> contains a colon <c>:</c>, or slashes ___</exception>
    let ofStr (str: string) =
        match tryOfStr str with
        | Some name -> name
        | None -> invalidArg "str" "The assembly name was empty or contains invalid characters."
