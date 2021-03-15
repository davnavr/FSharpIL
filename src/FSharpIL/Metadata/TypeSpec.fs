namespace FSharpIL.Metadata

[<Interface>] type internal ITypeSpec = inherit IIndexValue

/// <summary>Represents a row in the <c>TypeSpec</c> table (II.22.39)</summary>
[<System.Runtime.CompilerServices.IsReadOnly>]
[<NoComparison; StructuralEquality>]
type TypeSpecRow = struct
    val internal Type: ITypeSpec // Signature

    internal new (typeSpec: ITypeSpec) = { Type = typeSpec }

    override this.ToString() = this.Type.ToString()

    member internal this.CheckOwner owner = IndexOwner.checkOwner owner this.Type

    interface IIndexValue with member this.CheckOwner owner = this.CheckOwner owner
end

/// <category>Errors</category>
[<Sealed>]
type DuplicateTypeSpecError (typeSpec: TypeSpecRow) =
    inherit ValidationError()
    member _.TypeSpec = typeSpec
