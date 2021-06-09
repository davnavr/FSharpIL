namespace FSharpIL.Writing

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CliHeaderBuilder =
    { MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Requires32Bit: bool }

[<RequireQualifiedAccess>]
module CliHeaderBuilder =
    let defaultFields =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Requires32Bit = false }
