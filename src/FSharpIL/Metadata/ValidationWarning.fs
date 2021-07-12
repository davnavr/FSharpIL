namespace FSharpIL.Metadata

open System.Collections.Generic

/// <summary>Marker interface used to represent a <c>WARNING</c> check (II.22.1).</summary>
/// <category>Warnings</category>
type IValidationWarning = interface end

type ValidationWarningsBuilder = ICollection<IValidationWarning>

/// A read-only collection containing the warnings produced during validation of metadata.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ValidationWarningsCollection internal (?warnings: ValidationWarningsBuilder) =
    member _.Count =
        match warnings with
        | Some warnings' -> warnings'.Count
        | None -> 0

    member _.Contains warning =
        match warnings with
        | Some warnings' -> warnings'.Contains warning
        | None -> false

    member _.GetEnumerator() =
        match warnings with
        | Some warnings' -> warnings'.GetEnumerator()
        | None -> Seq.empty.GetEnumerator()

    interface IReadOnlyCollection<IValidationWarning> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
