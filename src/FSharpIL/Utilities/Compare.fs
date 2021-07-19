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
    let inline equals<'X, 'Y when 'X :> IEquatable<'Y>> (x: 'X) (y: 'Y) = x.Equals(other = y)

    let inline sequences x y =
        let mutable xenumerator = (^Sequence : (member GetEnumerator: unit -> ^Enumerator) x)
        let mutable yenumerator = (^Sequence : (member GetEnumerator: unit -> ^Enumerator) x)
        let mutable eq, cont = true, true
        while eq && cont do
            let mutable xmoved = (^Enumerator : (member MoveNext: unit -> bool) xenumerator)
            let mutable ymoved = (^Enumerator : (member MoveNext: unit -> bool) yenumerator)

            cont <- xmoved && ymoved

            if xmoved = ymoved then
                if cont then
                    eq <-
                        equals (^Enumerator : (member Current: ^T) xenumerator) (^Enumerator : (member Current: ^T) yenumerator)
            else eq <- false
        eq

    let lists (x: #IList<'X>) (y: #IList<'Y>) =
        let mutable eq, i = x.Count = y.Count, 0
        while eq && i < x.Count do
            eq <- equals x.[i] y.[i]
            i <- i + 1
        eq

    let inline blocks (x: ImmutableArray<'X>) (y: ImmutableArray<'Y>) = (x.IsDefaultOrEmpty && y.IsDefaultOrEmpty) || lists x y

    let inline voption (x: 'X voption) (y: 'Y voption) =
        match x, y with
        | ValueSome x', ValueSome y' -> equals x' y'
        | ValueNone, ValueNone -> true
        | _ -> false

    type BlockComparer<'T when 'T :> IEquatable<'T>> =
        | BlockComparer

        interface IEqualityComparer<ImmutableArray<'T>> with
            member _.Equals(x, y) = blocks x y
            member _.GetHashCode obj =
                let mutable hcode = HashCode()

                if not obj.IsDefaultOrEmpty then
                    for i = 0 to obj.Length - 1 do hcode.Add obj.[i] // NOTE: Hopefully this doesn't cause too much struct copying

                hcode.ToHashCode()

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
    let inline comparison<'X, 'Y when 'X :> IComparable<'Y>> (x: 'X) (y: 'Y) = x.CompareTo y

/// <summary>Less-than operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (<.) x y = Comparable.comparison x y < 0

/// <summary>Less-than-or-equal operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (<=.) x y = Comparable.comparison x y <= 0

/// <summary>Greater-than operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (.>) x y = Comparable.comparison x y > 0

/// <summary>Greater-than-or-equal operator for objects implementing <see cref="T:System.IComparable`1"/>.</summary>
let inline (.>=) x y = Comparable.comparison x y >= 0
