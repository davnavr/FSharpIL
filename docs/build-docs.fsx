#r "nuget: FSharp.Formatting"

open System.IO

let content = Path.Combine(__SOURCE_DIRECTORY__, "content")

printfn "%s" content
