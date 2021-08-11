[<AutoOpen>]
module internal FSharpIL.Utilities.ObjectPatterns

let inline (|ToString|) (value: 'T) = value.ToString()

let inline (|HashCodeOf|) (value: 'T) = value.GetHashCode()
