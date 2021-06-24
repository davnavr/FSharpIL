namespace FSharpIL.Cli // TODO: Rename namespace to FSharpIL.CommonLanguageInfrastructure or FSharpIL.CommonTypeSystem or FSharpIL.Cts

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

/// Specifies the visibility of a defined member.
[<IsReadOnly; Struct>]
type MemberVisibility =
    | CompilerControlled
    /// The containing type can access this member.
    | Private
    | Public
    /// The containing type and its derived types can access this member.
    | Family
    /// Types in the same assembly as the containing type can access this member.
    | Assembly
    /// Types in the same assembly that are derived from the containing type can access this member.
    | FamilyAndAssembly
    /// Types in the same assembly as or types derived from the containing type can access this member.
    | FamilyOrAssembly

[<RequireQualifiedAccess>]
module MemberVisibility =
    let inline ofMethod (visibility: MemberVisibility) =
        match visibility with
        | CompilerControlled -> MethodDefFlags.CompilerControlled
        | Private -> MethodDefFlags.Private
        | Public -> MethodDefFlags.Public
        | Family -> MethodDefFlags.Family
        | Assembly -> MethodDefFlags.Assembly
        | FamilyAndAssembly -> MethodDefFlags.FamAndAssem
        | FamilyOrAssembly -> MethodDefFlags.FamOrAssem

    let inline ofField (visibility: MemberVisibility) =
        match visibility with
        | CompilerControlled -> FieldFlags.CompilerControlled
        | Private -> FieldFlags.Private
        | Public -> FieldFlags.Public
        | Family -> FieldFlags.Family
        | Assembly -> FieldFlags.Assembly
        | FamilyAndAssembly -> FieldFlags.FamAndAssem
        | FamilyOrAssembly -> FieldFlags.FamOrAssem

/// Specifies the visibility of a referenced member or type.
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ExternalVisibility =
    | Public
    | Family

[<RequireQualifiedAccess>]
module ExternalVisibility =
    let inline asMemberVisibility visibility =
        match visibility with
        | ExternalVisibility.Public -> Public
        | ExternalVisibility.Family -> Family

    let inline ofMethod visibility =
        match visibility with
        | ExternalVisibility.Public -> MethodDefFlags.Public
        | ExternalVisibility.Family -> MethodDefFlags.Family
