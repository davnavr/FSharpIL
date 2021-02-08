module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable

val internal write : PEFile -> int32 * System.Collections.Generic.LinkedList<byte[]>

/// Creates a byte array containing the Portable Executable file.
val toArray : PEFile -> byte[]

/// <summary>
/// Creates a <see cref="T:System.IO.Stream"/> used to read over the Portable Executable file.
/// </summary>
val stream : PEFile -> Stream
