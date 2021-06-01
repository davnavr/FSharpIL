[<RequireQualifiedAccess>]
module FSharpIL.Writing.WritePE

open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open FSharpIL
open FSharpIL.PortableExecutable

val internal write : PEFile -> #IByteWriter -> struct(int32 * LinkedList<byte[]>)

/// Writes the Portable Executable file to an immutable byte array.
val block : PEFile -> ImmutableArray<byte>

val chunkedMemory : PEFile -> ChunkedMemory

/// <summary>
/// Creates a read-only <see cref="T:System.IO.Stream"/> used to read over the Portable Executable file.
/// </summary>
val stream : PEFile -> Stream

val toArray : PEFile -> byte[] -> unit

/// <summary>
/// Writes the Portable Executable to the specified file.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="file"/> is <see langword="null"/>.</exception>
val toFile : file: FileInfo -> PEFile -> unit

/// <summary>
/// Writes the Portable Executable to a file specified by the path.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="path"/> is <see langword="null"/>.</exception>
val toPath : path: string -> PEFile -> unit

/// <summary>
/// Writes the Portable Executable file to the specified <see cref="T:System.IO.Stream"/> and closes it.
/// </summary>
/// <exception cref="T:System.ArgumentException">The <paramref name="stream"/> does not support writing.</exception>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="stream"/> is <see langword="null"/>.</exception>
val toStream : stream: Stream -> PEFile -> unit
