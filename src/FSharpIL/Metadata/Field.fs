namespace FSharpIL.Metadata

open System.Runtime.CompilerServices
open System.Reflection

type [<AbstractClass; Sealed>] InstanceFieldTag = class end
type [<AbstractClass; Sealed>] StaticFieldTag = class end

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type Field<'Flags> =
    { Flags: ValidFlags<'Flags, FieldAttributes> 
      FieldName: Identifier
      Signature: Blob<FieldSignature> }

    member internal this.Row() = FieldRow(this.Flags.Value, this.FieldName, this.Signature)

/// <summary>
/// Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/> defined inside of the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
type GlobalField = unit

/// <summary>Represents a non-static field in the <c>Field</c> table.</summary>
type InstanceField = Field<InstanceFieldTag>

/// <summary>Represents a static field in the <c>Field</c> table.</summary>
type StaticField = Field<StaticFieldTag>

// NOTE: Prevent setting of InitOnly field for fields marked Literal
// type LiteralField

// NOTE: Static fields should also be allowed in interfaces.
