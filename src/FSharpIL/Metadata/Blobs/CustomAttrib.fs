namespace FSharpIL.Metadata.Blobs

open System.Runtime.CompilerServices
open System.Collections.Immutable

/// <summary>Represents an <c>Elem</c> item, which is an argument in a custom attribute (II.23.3).</summary>
type Elem =
    | ValBool of bool
    | ValChar of char
    | ValR4 of float32
    | ValR8 of System.Double
    | ValI1 of int8
    | ValU1 of uint8
    | ValI2 of int16
    | ValU2 of uint16
    | ValI4 of int32
    | ValU4 of uint32
    | ValI8 of int64
    | ValU8 of uint64
    // | ValEnum // of SomehowGetTheEnumUnderlyingType?
    /// <summary>Represents a string used as an argument in a custom attribute.</summary>
    /// <remarks>Empty strings and <see langword="null"/> strings are allowed values.</remarks>
    | SerString of string
    // | SerStringType // of SomehowGetTheCanonicalNameOfType.
    // | BoxedObject of // underlying value.

/// <summary>
/// Represents a <c>FixedArg</c> item, which stores the arguments for a custom attribute's constructor method (II.23.3).
/// </summary>
[<RequireQualifiedAccess>]
type FixedArg =
    | Elem of Elem
    | SZArray of ImmutableArray<Elem voption>

[<IsReadOnly; Struct>]
type NamedArg =
    { IsProperty: bool
      Name: string
      Value: FixedArg }

[<RequireQualifiedAccess>]
module NamedArg =
    let inline (|Field|Prop|) arg =
        let value() = struct(arg.Name, arg.Value)
        if arg.IsProperty then Prop(value()) else Field(value())
    let inline Field (name, value) = { IsProperty = false; Name = name; Value = value }
    let inline Prop (name, value) = { IsProperty = true; Name = name; Value = value }

/// <summary>
/// Represents a <c>CustomAttrib</c> item, which stores the arguments provided to a custom attribute's constructor, as well as
/// any values assigned to its fields or properties. (II.23.3).
/// </summary>
[<IsReadOnly; Struct>]
type CustomAttrib = { FixedArgs: ImmutableArray<FixedArg>; NamedArgs: ImmutableArray<NamedArg> }
