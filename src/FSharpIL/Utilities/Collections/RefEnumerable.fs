namespace FSharpIL.Utilities.Collections

type internal IRefEnumerator<'Item when 'Item : struct> = interface
    abstract Current: inref<'Item>
    abstract MoveNext: unit -> bool
end

type internal IRefEnumerable<'Item, 'Enumerator when 'Item : struct and 'Enumerator :> IRefEnumerator<'Item> and 'Enumerator : struct> = interface
    abstract GetEnumerator: unit -> 'Enumerator
end
