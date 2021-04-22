namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

/// <summary>
/// Represents a <c>FieldSig</c> item, which captures the definition of a field or global variable (II.23.2.4).
/// </summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type FieldSignature = struct // TODO: Move to Items.fs, because of Blob`1
    val CustomMod: ImmutableArray<CustomModifier>
    val internal Type: IEncodedType // FieldType
    internal new (modifiers, fieldType) = { CustomMod = modifiers; Type = fieldType }
    override this.ToString() = this.Type.ToString()
end

//type FieldSig = class end

/// <summary>Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags, name, signature) =
    member _.Flags: FieldAttributes = flags
    member _.Name: Identifier = name
    member _.Signature: Blob<FieldSignature> = signature

    member internal _.SkipDuplicateChecking = flags &&& FieldAttributes.FieldAccessMask = FieldAttributes.PrivateScope

    override this.Equals obj =
        match obj with
        | :? FieldRow as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, signature)

    interface IEquatable<FieldRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else name = other.Name && signature = other.Signature

/// <summary>Error used when there is a duplicate row in the <c>Field</c> table (17).</summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateFieldError (field: FieldRow) =
    inherit ValidationError()
    member _.Field = field
