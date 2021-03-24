namespace global

open FSharpIL.Metadata

[<AutoOpen>]
module ItemExtensions =
    type CustomModifier with
        member this.ModifierType = this.CMod :?> TypeDefOrRefOrSpecEncoded

    type ReturnTypeItem with
        member this.ReturnType =
            match this.RetType with
            | :? ReturnType as rtype -> rtype
            | :? ReturnTypeVoid -> ReturnType.Void
            | unknown -> failwithf "Unknown return type %A" unknown

    type TypeSpecRow with
        member this.Signature = this.Type :?> TypeSpec

    type MethodSpecRow with
        member this.Instantiation = this.Item :?> MethodSpec

    type ParamItem with
        member this.ParamType = this.Type :?> EncodedType

    type FieldSignature with
        member this.FieldType = this.Type :?> EncodedType

    type LocalVariable with
        member this.LocalType = this.Type :?> EncodedType

namespace FSharpIL.Metadata

open System

[<AutoOpen>]
module ItemHelpers =
    let inline (|OptionalCustomModifier|RequiredCustomModifier|) (cmod: CustomModifier) =
        let modifierType = cmod.ModifierType
        if cmod.Required
        then RequiredCustomModifier modifierType
        else OptionalCustomModifier modifierType

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LocalVariable =
    let (|Type|ByRef|TypedByRef|) (localvar: LocalVariable) =
        match localvar.Tag with
        | LocalVariableTag.Type -> Type
        | LocalVariableTag.ByRef -> ByRef
        | LocalVariableTag.TypedByRef -> TypedByRef
        | _ -> invalidArg "localvar" "Invalid local variable type"

    let Type modifiers constraints (ltype: EncodedType) =
        LocalVariable(LocalVariableTag.Type, modifiers, constraints, ltype)
    let ByRef modifiers constraints (ltype: EncodedType) =
        LocalVariable(LocalVariableTag.ByRef, modifiers, constraints, ltype)
    let TypedByRef modifiers constraints (ltype: EncodedType) =
        LocalVariable(LocalVariableTag.TypedByRef, modifiers, constraints, ltype)
