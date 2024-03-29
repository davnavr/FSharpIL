﻿namespace FSharpIL.Metadata

open System
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Compare

/// <summary>Represents an offset into the <c>#Strings</c> metadata stream pointing to a non-empty string.</summary>
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type IdentifierOffset =
    internal { Offset: StringOffset }

/// <summary>Represents an offset into the <c>#Strings</c> metadata stream pointing to the name of an assembly or file.</summary>
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type FileNameOffset =
    internal { Offset: IdentifierOffset }

/// <summary>
/// Represents a <see cref="T:System.String"/> that cannot be <see langword="null"/>, be empty, or contain any null characters.
/// </summary>
[<IsReadOnly; Struct; CustomComparison; CustomEquality>]
type Identifier =
    val private identifier: string

    internal new (identifier) = { identifier = identifier }

    member this.AsMemory() = this.identifier.AsMemory()

    override this.ToString() = this.identifier

    static member (+) (x: Identifier, y: Identifier) = Identifier(x.identifier + y.identifier)

    static member (+) (id: Identifier, str) =
        match str with
        | null
        | "" -> id
        | _ -> Identifier(id.identifier + str)

    override this.GetHashCode() = StringComparer.Ordinal.GetHashCode this.identifier

    interface IEquatable<Identifier> with
        member this.Equals other = StringComparer.Ordinal.Equals(this.identifier, other.identifier)

    interface IComparable<Identifier> with
        member this.CompareTo other = StringComparer.Ordinal.Compare(this.identifier, other.identifier)

    interface IComparable with member this.CompareTo obj = Comparable.comparison this (obj :?> Identifier)

    override this.Equals obj =
        match obj with
        | :? Identifier as other -> this === other
        | _ -> false

/// Represents the name of an assembly (II.22.2) or file (II.22.19).
[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
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
    let inline private invalidFileName() = invalidArg "str" "The assembly name was empty or contains invalid characters."

    let tryOfId (id: Identifier) =
        if id.ToString().IndexOfAny [| ':'; '\\'; '/' |] = -1
        then ValueSome { FileName = id }
        else ValueNone

    let tryOfStr (str: string) =
        match Identifier.tryOfStr str with
        | ValueSome name -> tryOfId name
        | ValueNone -> ValueNone

    /// <summary>Creates a name for an <c>Assembly</c> or <c>AssemblyRef</c>.</summary>
    /// <exception cref="T:System.ArgumentException">
    /// The name is empty or contains a colon, a forward slash, or a backslash character.
    /// </exception>
    let ofStr (str: string) =
        match tryOfStr str with
        | ValueSome name -> name
        | ValueNone -> invalidFileName()

    let ofId id =
        match tryOfId id with
        | ValueSome name -> name
        | ValueNone -> invalidFileName()

[<AutoOpen>]
module StringOffsetPatterns =
    let (|IdentifierOffset|) { IdentifierOffset.Offset = offset } = offset
    let (|FileNameOffset|) { FileNameOffset.Offset = offset } = offset.Offset
