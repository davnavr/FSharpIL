module internal FSharpIL.Utilities.Compare

open System
open System.Collections.Generic
open System.Collections.Immutable

/// <summary>
/// Contains functions for comparing objects and collections containing objects that implement
/// <see cref="T:System.IEquatable`1"/>.
/// </summary>
[<RequireQualifiedAccess>]
module Equatable =
    let inline equals<'T when 'T :> IEquatable<'T>> (x: 'T) (y: 'T) = x.Equals(other = y)

    let lists (x: #IList<'T>) (y: #IList<'T>) =
        let mutable eq, i = x.Count = y.Count, 0
        while eq && i < x.Count do
            eq <- equals x.[i] y.[i]
            i <- i + 1
        eq

    let inline blocks (x: ImmutableArray<'T>) (y: ImmutableArray<'T>) = (x.IsDefaultOrEmpty && y.IsDefaultOrEmpty) || lists x y

    let inline voption (x: 'T voption) (y: 'T voption) =
        match x, y with
        | ValueSome x', ValueSome y' -> equals x' y'
        | ValueNone, ValueNone -> true
        | _ -> false

/// <summary>Equality operator for objects implementing <see cref="T:System.IEquatable`1"/>.</summary>
let inline (===) x y = Equatable.equals x y

/// <summary>
/// Contains functions for comparing objects and collections containing objects that implement
/// <see cref="T:System.IComparable`1"/>.
/// </summary>
[<RequireQualifiedAccess>]
module Comparable =
    /// <returns>
    /// A value less than zero if <paramref name="x"/> comes before <paramref name="y"/>,
    /// a value greater than zero if <paramref name="x"/> comes after <paramref name="y"/>,
    /// or zero if <paramref name="x"/> is equal to <paramref name="y"/>.
    /// </returns>
    let inline comparison<'T when 'T :> IComparable<'T>> (x: 'T) (y: 'T) = x.CompareTo y

/// <summary>Less-than operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (<.) x y = Comparable.comparison x y < 0

/// <summary>Less-than-or-equal operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (<=.) x y = Comparable.comparison x y <= 0

/// <summary>Greater-than operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (.>) x y = Comparable.comparison x y > 0

/// <summary>Greater-than-or-equal operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (.>=) x y = Comparable.comparison x y >= 0
