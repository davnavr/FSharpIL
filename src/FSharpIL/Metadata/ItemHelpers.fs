[<AutoOpen>]
module FSharpIL.Metadata.ItemsExtensions // TODO: Make extensions globally available.

type CustomModifier with
    member this.ModifierType = this.CMod :?> TypeDefOrRefOrSpecEncoded

let inline (|OptionalCustomModifier|RequiredCustomModifier|) (cmod: CustomModifier) =
    let modifierType = cmod.ModifierType
    if cmod.Required
    then RequiredCustomModifier modifierType
    else OptionalCustomModifier modifierType

type ReturnTypeItem with
    member this.ReturnType = this.RetType :?> ReturnType

type TypeSpecRow with
    member this.Signature = this.Type :?> TypeSpec

type MethodSpecRow with
    member this.Instantiation = this.Item :?> MethodSpec

type ParamItem with
    member this.ParamType = this.Type :?> EncodedType

type FieldSignature with
    member this.FieldType = this.Type :?> EncodedType
