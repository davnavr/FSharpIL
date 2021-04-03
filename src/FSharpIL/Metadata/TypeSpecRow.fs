namespace FSharpIL.Metadata

open System.Runtime.CompilerServices

[<IsReadOnly>]
type TypeSpecBlob = struct
    val internal Index: int32
    internal new (index) = { Index = index }
end

/// <summary>Represents a row in the <c>TypeSpec</c> table (II.22.39)</summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type TypeSpecRow = struct
    val Signature: TypeSpecBlob
    new (signature) = { Signature = signature }
end

/// <category>Errors</category>
[<Sealed>]
type DuplicateTypeSpecError (typeSpec: TypeSpecRow) =
    inherit ValidationError()
    member _.TypeSpec = typeSpec
