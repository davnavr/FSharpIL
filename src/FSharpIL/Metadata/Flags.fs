namespace FSharpIL.Metadata

open System
open System.Reflection
open System.Runtime.CompilerServices

type IFlags<'Flags when 'Flags :> Enum> = abstract Value: 'Flags

[<IsReadOnly>]
type ValidFlags<'Tag, 'Flags when 'Flags :> Enum> = struct
    val Value: 'Flags
    internal new(flags: 'Flags) = { Value = flags }
    override this.ToString() = this.Value.ToString()
    interface IFlags<'Flags> with member this.Value = this.Value
end

[<AutoOpen>]
module internal FlagPatterns =
    let (|Flags|) (flags: #IFlags<_>) = flags.Value

type Visibility =
    | CompilerControlled
    | Private
    | FamilyAndAssembly
    | Assembly
    | Family
    | FamilyOrAssembly
    | Public

    interface IFlags<FieldAttributes> with
        member this.Value =
            match this with
            | CompilerControlled -> FieldAttributes.PrivateScope
            | Private -> FieldAttributes.Private
            | FamilyAndAssembly -> FieldAttributes.FamANDAssem
            | Assembly -> FieldAttributes.Assembly
            | Family -> FieldAttributes.Family
            | FamilyOrAssembly -> FieldAttributes.FamORAssem
            | Public -> FieldAttributes.Public

    interface IFlags<MethodAttributes> with
        member this.Value =
            match this with
            | CompilerControlled -> MethodAttributes.PrivateScope
            | Private -> MethodAttributes.Private
            | FamilyAndAssembly -> MethodAttributes.FamANDAssem
            | Assembly -> MethodAttributes.Assembly
            | Family -> MethodAttributes.Family
            | FamilyOrAssembly -> MethodAttributes.FamORAssem
            | Public -> MethodAttributes.Public

/// <summary>
/// Visibility for fields and methods defined in the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
[<RequireQualifiedAccess>]
type GlobalVisibility =
    | Public
    | CompilerControlled
    | Private

    interface IFlags<FieldAttributes> with
        member this.Value =
            match this with
            | Public -> FieldAttributes.Public
            | CompilerControlled -> FieldAttributes.PrivateScope
            | Private -> FieldAttributes.Private

type internal SpecialNameTag =
    | None = 0uy
    | Special = 1uy
    | RTSpecial = 2uy

[<IsReadOnly; Struct>]
type SpecialName internal (tag: SpecialNameTag) =
    member internal _.Tag = tag
    interface IFlags<FieldAttributes> with
        member this.Value =
            match this.Tag with
            | SpecialNameTag.Special -> FieldAttributes.SpecialName
            | SpecialNameTag.RTSpecial -> FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName
            | SpecialNameTag.None
            | _ -> FieldAttributes.PrivateScope
    interface IFlags<MethodAttributes> with
        member this.Value =
            match this.Tag with
            | SpecialNameTag.Special -> MethodAttributes.SpecialName
            | SpecialNameTag.RTSpecial -> MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName
            | SpecialNameTag.None
            | _ -> MethodAttributes.PrivateScope
    interface IFlags<EventAttributes> with
        member this.Value =
            match this.Tag with
            | SpecialNameTag.Special -> EventAttributes.SpecialName
            | SpecialNameTag.RTSpecial -> EventAttributes.RTSpecialName ||| EventAttributes.SpecialName
            | SpecialNameTag.None
            | _ -> EventAttributes.None

[<AutoOpen>]
module SpecialName =
    let (|NoSpecialName|SpecialName|RTSpecialName|) (specialName: SpecialName) =
        match specialName.Tag with
        | SpecialNameTag.Special -> SpecialName
        | SpecialNameTag.RTSpecial -> RTSpecialName
        | SpecialNameTag.None
        | _ -> NoSpecialName

    /// <summary>Leaves both the <c>SpecialName</c> and <c>RTSpecialName</c> flags clear.</summary>
    let NoSpecialName = SpecialName SpecialNameTag.None
    /// <summary>Sets both the <c>SpecialName</c> and <c>RTSpecialName</c> flags.</summary>
    let RTSpecialName = SpecialName SpecialNameTag.RTSpecial
    /// <summary>Sets the <c>SpecialName</c> flag, and leaves the <c>RTSpecialName</c> flag clear.</summary>
    let SpecialName = SpecialName SpecialNameTag.Special
