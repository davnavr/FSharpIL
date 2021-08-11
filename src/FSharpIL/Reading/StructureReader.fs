namespace FSharpIL.Reading

open FSharpIL.PortableExecutable

type StructureReader<'Structure, 'State> = ('Structure -> FileOffset -> 'State -> 'State voption) voption

[<RequireQualifiedAccess>]
module internal StructureReader =
    let inline read (reader: StructureReader<_, _>) arg offset ustate next =
        match reader with
        | ValueSome reader' ->
            match reader' arg offset ustate with
            | ValueSome ustate' -> Success(ustate', next)
            | ValueNone -> End
        | ValueNone -> Success(ustate, next)
