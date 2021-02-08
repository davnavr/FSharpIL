module FSharpIL.WritePE

open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open FSharpIL.PortableExecutable

val internal write : PEFile -> int32 * LinkedList<byte[]>

/// Creates a byte array containing the Portable Executable file.
val toArray : PEFile -> byte[]

/// <summary>
/// Writes the Portable Executable file to an immutable byte array.
/// </summary>
val toBlock : PEFile -> ImmutableArray<byte>

/// <summary>
/// Creates a <see cref="T:System.IO.Stream"/> used to read over the Portable Executable file.
/// </summary>
val stream : PEFile -> Stream

/// <summary>
/// Writes the Portable Executable file to the specified <see cref="T:System.IO.Stream"/> and closes it.
/// </summary>
/// <exception cref="T:System.ArgumentException">The <paramref name="stream"/> does not support writing.</exception>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="stream"/> is <see langword="null"/>.</exception>
val toStream : PEFile -> stream: Stream -> unit

/// <summary>
/// Writes the Portable Executable to the specified file.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="file"/> is <see langword="null"/>.</exception>
val toFile : PEFile -> file: FileInfo -> unit

/// <summary>
/// Writes the Portable Executable to a file specified by the path.
/// </summary>
/// <exception cref="T:System.ArgumentNullException">The <paramref name="path"/> is <see langword="null"/>.</exception>
val toPath : PEFile -> path: string -> unit
