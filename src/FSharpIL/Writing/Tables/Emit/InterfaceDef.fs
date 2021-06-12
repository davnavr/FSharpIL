namespace FSharpIL.Writing.Tables.Emit

open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type InterfaceDef =
    { Access: TypeVisibility
      InterfaceName: Identifier
      Namespace: string }
