namespace global

open System.Runtime.CompilerServices

open FSharpIL.Metadata

[<AutoOpen>]
module ItemExtensions =
    type ReturnTypeItem with
        member this.ReturnType =
            match this.RetType with
            | :? ReturnType as rtype -> rtype
            | :? ReturnTypeVoid -> ReturnType.Void
            | unknown -> failwithf "Unknown return type %A" unknown

    type FieldSignature with
        member this.FieldType = Unsafe.As<EncodedType> this.Type

    type LocalVariable with
        member this.LocalType = Unsafe.As<EncodedType> this.Type

namespace FSharpIL.Metadata

open System.Collections.Immutable

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
    /// Represent a local variable containing a managed pointer (II.14.4.2).
    let ByRef modifiers constraints (ltype: EncodedType) =
        LocalVariable(LocalVariableTag.ByRef, modifiers, constraints, ltype)
    let TypedByRef modifiers constraints (ltype: EncodedType) =
        LocalVariable(LocalVariableTag.TypedByRef, modifiers, constraints, ltype)

    let encoded (etype: EncodedType) = Type ImmutableArray.Empty ImmutableArray.Empty etype
