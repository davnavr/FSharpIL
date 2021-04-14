namespace FSharpIL.Metadata

[<RequireQualifiedAccess>]
module internal EventHelpers =
    let eventType =
        function
        | EventType.TypeRef tref -> EncodedType.typeRefClass tref
        | bad -> failwithf "Unsupported event type %A" bad

    let parameters = ParamList.named [| "handler" |]

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type GeneratedEvent internal (row: RawIndex<EventRow>, add: RawIndex<MethodDefRow>, remove: RawIndex<MethodDefRow>) =
    member _.Row = row
    member _.Add = add
    member _.Remove = remove
