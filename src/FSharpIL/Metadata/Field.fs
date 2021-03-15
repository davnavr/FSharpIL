namespace FSharpIL.Metadata

open System.Runtime.CompilerServices
open System.Reflection

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type Field<'Flags> =
    { Flags: ValidFlags<'Flags, FieldAttributes>
      FieldName: Identifier
      Signature: FieldSignature }

    member internal this.CheckOwner actual = this.Signature.CheckOwner actual
    member internal this.Row() = FieldRow(this.Flags.Value, this.FieldName, this.Signature)

/// <summary>
/// Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/> defined inside of the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
type GlobalField = unit

/// <summary>Represents a non-static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
[<Sealed>]
type InstanceField (field: Field<InstanceField>) =
    interface IIndexValue with member _.CheckOwner owner = field.CheckOwner owner
    interface IField<ConcreteClassDef> with member _.Row() = field.Row()
    interface IField<AbstractClassDef> with member _.Row() = field.Row()
    interface IField<SealedClassDef> with member _.Row() = field.Row()

/// <summary>Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
[<Sealed>]
type StaticField (field: Field<StaticField>) =
    interface IIndexValue with member _.CheckOwner owner = field.CheckOwner owner
    interface IField<ConcreteClassDef> with member _.Row() = field.Row()
    interface IField<AbstractClassDef> with member _.Row() = field.Row()
    interface IField<SealedClassDef> with member _.Row() = field.Row()
    interface IField<StaticClassDef> with member _.Row() = field.Row()

    interface IField<StructDef> with member _.Row() = field.Row()

    // NOTE: Static fields should also be allowed in interfaces.

// TODO: Make field union type to avoid explicit casting with :>
