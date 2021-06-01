[<RequireQualifiedAccess>]
module FSharpIL.Writing.WritePE

open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open FSharpIL
open FSharpIL.PortableExecutable

// TODO: Use PEFile instead of #IPortableExecutable

val internal write : #IPortableExecutable -> #IByteWriter -> struct(int32 * LinkedList<byte[]>)

/// Writes the Portable Executable file to an immutable byte array.
val block : #IPortableExecutable -> ImmutableArray<byte>

val chunkedMemory : #IPortableExecutable -> ChunkedMemory

/// <summary>
/// Creates a read-only <see cref="T:System.IO.Stream"/> used to read over the Portable Executable file.
/// </summary>
val stream : #IPortableExecutable -> Stream

val toArray : #IPortableExecutable -> byte[] -> unit

/// <summary>
/// Writes the Portable Executable to the specified file.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="file"/> is <see langword="null"/>.</exception>
val toFile : file: FileInfo -> #IPortableExecutable -> unit

/// <summary>
/// Writes the Portable Executable to a file specified by the path.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="path"/> is <see langword="null"/>.</exception>
val toPath : path: string -> #IPortableExecutable -> unit

/// <summary>
/// Writes the Portable Executable file to the specified <see cref="T:System.IO.Stream"/> and closes it.
/// </summary>
/// <exception cref="T:System.ArgumentException">The <paramref name="stream"/> does not support writing.</exception>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="stream"/> is <see langword="null"/>.</exception>
val toStream : stream: Stream -> #IPortableExecutable -> unit
