namespace FSharpIL.Cli

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
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
    let ofMethod (visibility: MemberVisibility) =
        match visibility with
        | CompilerControlled -> MethodDefFlags.CompilerControlled
        | Private -> MethodDefFlags.Private
        | Public -> MethodDefFlags.Public
        | Family -> MethodDefFlags.Family
        | Assembly -> MethodDefFlags.Assembly
        | FamilyAndAssembly -> MethodDefFlags.FamAndAssem
        | FamilyOrAssembly -> MethodDefFlags.FamOrAssem

    let ofField (visibility: MemberVisibility) =
        match visibility with
        | CompilerControlled -> FieldFlags.CompilerControlled
        | Private -> FieldFlags.Private
        | Public -> FieldFlags.Public
        | Family -> FieldFlags.Family
        | Assembly -> FieldFlags.Assembly
        | FamilyAndAssembly -> FieldFlags.FamAndAssem
        | FamilyOrAssembly -> FieldFlags.FamOrAssem
