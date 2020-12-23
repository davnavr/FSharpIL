[<RequireQualifiedAccess; System.ObsoleteAttribute>]
module internal FSharpIL.Utilities.Enum

let from value = LanguagePrimitives.EnumOfValue<'num, 'enum> value
