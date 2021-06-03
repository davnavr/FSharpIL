namespace FSharpIL.Reading

type StructureReader<'Structure, 'State> = ('Structure -> FileOffset -> 'State -> 'State voption) voption

[<RequireQualifiedAccess>]
module StructureHeader =
    let skip (_: FileOffset) state = ValueSome state

    let inline read (reader: StructureReader<_, _>) arg =
        match reader with
        | ValueSome reader' -> reader' arg
        | ValueNone -> skip
