namespace FSharpIL.Metadata

open System
open System.Collections.Generic

/// <summary>(0x26) Represents a row in the <c>File</c> table (II.22.19).</summary>
[<CustomComparison; CustomEquality>]
type File =
    { /// <summary>
      /// Corresponds to the <c>Flags</c> row, indicates whether a file "is a resource file or
      /// other non-metadata-containing file" (II.23.1.6).
      /// </summary>
      ContainsMetadata: bool
      FileName: Identifier
      HashValue: byte[] }

    override this.Equals obj = (this :> IEquatable<File>).Equals(obj :?> File)
    override this.GetHashCode() = this.FileName.GetHashCode()

    interface IEquatable<File> with
        member this.Equals other = this.FileName = other.FileName

    interface IComparable with
        member this.CompareTo obj = compare this.FileName (obj :?> File).FileName

// TODO: Fix, shares code with AssemblyRefTable
[<Sealed>]
type FileTable internal (owner: IndexOwner) =
    let set = HashSet<File>()

    member _.Count = set.Count

    member _.GetIndex file =
        if set.Add file
        then SimpleIndex(owner, file) |> ValueSome
        else ValueNone

    interface IReadOnlyCollection<File> with
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator
