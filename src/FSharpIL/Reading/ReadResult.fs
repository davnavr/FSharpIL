namespace FSharpIL.Reading

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type internal ReadResult<'State, 'T, 'Error when 'State :> IReadState and 'State : struct> =
    | Success of 'T * 'State
    | Failure of 'Error
    | End
