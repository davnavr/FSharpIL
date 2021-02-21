[<AutoOpen>]
module FSharpIL.Metadata.ItemsExtensions

type CustomModifier with
    member this.ModifierType = this.CMod :?> TypeDefOrRefOrSpecEncoded

let inline (|OptionalCustomModifier|RequiredCustomModifier|) (cmod: CustomModifier) =
    let modifierType = cmod.ModifierType
    if cmod.Required
    then RequiredCustomModifier modifierType
    else OptionalCustomModifier modifierType

type ReturnTypeItem with
    member this.ReturnType = this.RetType :?> ReturnType

type ParamItem with
    member this.ParamType = this.Type :?> EncodedType
