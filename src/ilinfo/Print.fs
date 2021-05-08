[<RequireQualifiedAccess>]
module ILInfo.Print

open System
open System.Collections.Generic
open System.Reflection

open Microsoft.FSharp.Core.Printf

[<AbstractClass; Sealed>]
type private Enumeration<'Enum when 'Enum :> Enum and 'Enum : equality> () =
    static member val Cache =
        lazy
            let fields = typeof<'Enum>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
            let cache = Dictionary<'Enum, string>()
            for field in fields do
                let value = field.GetRawConstantValue()
                //let value' = value :?> uint64 // TODO: Check if it is a signed or unsigned integer.
                // If value is multiple by two, then add it
                // TODO: Check if value is multiple by two
                cache.[value :?> 'Enum] <- field.Name
            cache

let inline integer wr (value: 'Integer) = fprintf wr "0x%0*X" (2 * sizeof<'Integer>) value

let bitfield (wr: System.IO.TextWriter) (value: 'Enum when 'Enum :> Enum) =
    value.ToString("X")
    |> fprintf wr "0x%s "
    Seq.choose
        (fun (KeyValue(flag, name)) ->
            if value.HasFlag flag
            then Some name
            else None)
        Enumeration<'Enum>.Cache.Value
    |> String.concat ", "
    |> fprintf wr "[ %s ]"
