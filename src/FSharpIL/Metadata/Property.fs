namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type PropertySignature internal
    (
        hasThis: bool,
        customMod: ImmutableArray<CustomModifier>,
        propertyType: IEncodedType,
        parameters: ImmutableArray<ParamItem>
    ) =
    member _.HasThis = hasThis
    member _.CustomMod = customMod
    member internal _.Type = propertyType // PropertyType
    member _.Parameters = parameters

type IPropertySignature =
    abstract Signature: unit -> PropertySignature

/// <summary>(0x17) Represents a row in the <c>Property</c> table (II.22.34).</summary>
[<IsReadOnly>]
type PropertyRow = struct
    val Flags: PropertyAttributes
    val Name: Identifier
    val Type: PropertySignature
end

[<IsReadOnly; Struct>]
type Property<'Tag, 'Signature when 'Signature :> IPropertySignature> =
    { Flags: ValidFlags<'Tag, PropertyAttributes>
      /// <summary>Corresponds to the <c>Name</c> column of the <c>Property</c> table (II.22.34).</summary>
      PropertyName: Identifier
      Type: 'Signature }

[<IsReadOnly>]
type InstancePropertySignature = struct // TODO: Move EncodedType to a file further up so that it can be used in signature types such as here.
    val CustomMod: ImmutableArray<CustomModifier>
    val internal Type: IEncodedType
    val Parameters: ImmutableArray<ParamItem>

    interface IPropertySignature with
        member this.Signature() = PropertySignature(false, this.CustomMod, this.Type, this.Parameters)
end

type InstanceProperty = Property<InstanceMethodTag, InstancePropertySignature>

//type VirtualProperty = Property<>

[<IsReadOnly; Struct>]
type StaticPropertySignature internal (signature: InstancePropertySignature) =
    interface IPropertySignature with
        member _.Signature() = PropertySignature(false, signature.CustomMod, signature.Type, signature.Parameters)

type StaticProperty = Property<StaticMethodTag, StaticPropertySignature>
