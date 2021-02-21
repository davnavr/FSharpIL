namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

type IFlags<'Flags when 'Flags :> System.Enum> = abstract Value: 'Flags

[<IsReadOnly>]
type ValidFlags<'Tag, 'Flags when 'Flags :> System.Enum> =
    struct
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
