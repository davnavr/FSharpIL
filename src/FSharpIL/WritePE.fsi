module FSharpIL.WritePE

open FSharpIL.PortableExecutable

val internal write : PEFile -> int32 * System.Collections.Generic.LinkedList<byte[]>

val toArray : PEFile -> byte[]
