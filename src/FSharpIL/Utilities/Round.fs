[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Round

/// Rounds up the specified number up to a multiple of another number.
let inline upTo (multiple: ^T) (num: ^T): ^T =
    multiple * ((num + multiple - LanguagePrimitives.GenericOne) / multiple)
