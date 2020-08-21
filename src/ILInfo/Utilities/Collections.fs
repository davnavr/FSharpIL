module internal ILInfo.Utilities.Collections

open System.Collections.Immutable

module ImmList =
    let empty<'T> = ImmutableList<'T>.Empty
    let builder<'T> = ImmutableList.CreateBuilder<'T>
