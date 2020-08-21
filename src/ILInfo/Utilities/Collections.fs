module internal ILInfo.Utilities.Collections

open System.Collections.Immutable

module ImmList =
    let inline empty<'T> = ImmutableList<'T>.Empty
    let inline builder<'T> = ImmutableList.CreateBuilder<'T>
