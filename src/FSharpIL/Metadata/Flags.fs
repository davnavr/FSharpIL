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

type SpecialName =
    /// <summary>Leaves both the <c>SpecialName</c> and <c>RTSpecialName</c> flags clear.</summary>
    | NoSpecialName
    /// <summary>Sets the <c>SpecialName</c> flag, and leaves the <c>RTSpecialName</c> flag clear.</summary>
    | SpecialName
    /// <summary>Sets both the <c>SpecialName</c> and <c>RTSpecialName</c> flags.</summary>
    | RTSpecialName

    interface IFlags<FieldAttributes> with
        member this.Value =
            match this with
            | NoSpecialName -> FieldAttributes.PrivateScope
            | SpecialName -> FieldAttributes.SpecialName
            | RTSpecialName -> FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName

    interface IFlags<MethodAttributes> with
        member this.Value =
            match this with
            | NoSpecialName -> MethodAttributes.PrivateScope
            | SpecialName -> MethodAttributes.SpecialName
            | RTSpecialName -> MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName
