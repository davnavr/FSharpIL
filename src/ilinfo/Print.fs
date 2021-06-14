[<RequireQualifiedAccess>]
module ILInfo.Print

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open Microsoft.FSharp.Core.Printf

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<AbstractClass; Sealed>]
type private Enumeration<'Enum when 'Enum : struct and 'Enum :> Enum and 'Enum : equality> () =
    static member val Cache =
        lazy
            let fields = typeof<'Enum>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
            let cache = Dictionary<'Enum, string>()
            for field in fields do
                let value = field.GetRawConstantValue()
                // If value is multiple by two, then add it
                // TODO: Check if value is multiple by two
                // TODO: Check if it is a signed or unsigned integer.
                match (value :?> IConvertible).ToUInt64 null with
                | 0UL -> ()
                | _ -> cache.[value :?> 'Enum] <- field.Name
            cache

let inline integer wr (value: 'Integer) = fprintf wr "0x%0*X" (2 * sizeof<'Integer>) value

// TODO: List enum flags that are set vertically
let inline enumeration wr (value: 'Flag when 'Flag : enum<'Integer>) =
    fprintf wr "0x%0*X (%O)" sizeof<'Flag> (LanguagePrimitives.EnumToValue<_, 'Integer> value) value

let bitfield (wr: #TextWriter) (value: 'Enum when 'Enum :> Enum) =
    fprintf wr "0x%s " (value.ToString "X")

    Seq.choose
        (fun (KeyValue(flag, name)) ->
            if value.HasFlag flag
            then Some name
            else None)
        Enumeration<'Enum>.Cache.Value
    |> String.concat ", "
    |> fprintf wr "[ %s ]"

let inline uint32 wr int = integer wr (uint32 int)

let rvaAndSize wr { Rva = rva; Size = size } = fprintf wr "(RVA = %O, Size = 0x%08X)" rva size

let metadataToken wr (token: MetadataToken) =
    fprintf wr "0x%08X" token.Value
    if not token.IsNull then
        wr.Write " [ "
        wr.Write token
        wr.Write " ]"
