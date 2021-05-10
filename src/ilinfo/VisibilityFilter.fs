namespace ILInfo

open System.Reflection

type VisibilityFilter =
    | Public = 1uy
    | Private = 2uy
    | Family = 4uy
    | Assembly = 8uy
    | FamAndAssem = 0x10uy
    | FamOrAssem = 0x20uy
    | CompilerControlled = 0x40uy

[<RequireQualifiedAccess>]
module VisibilityFilter =
    let typeDef (vfilter: VisibilityFilter) flags =
        match flags &&& TypeAttributes.VisibilityMask with
        | TypeAttributes.NestedPublic 
        | TypeAttributes.Public -> vfilter.HasFlag VisibilityFilter.Public
        | TypeAttributes.NestedFamily -> vfilter.HasFlag VisibilityFilter.Family
        | TypeAttributes.NestedFamANDAssem -> vfilter.HasFlag VisibilityFilter.FamAndAssem
        | TypeAttributes.NestedFamORAssem ->  vfilter.HasFlag VisibilityFilter.FamOrAssem
        | TypeAttributes.NestedPrivate -> vfilter.HasFlag VisibilityFilter.Private
        | TypeAttributes.NotPublic
        | TypeAttributes.NestedAssembly
        | _ -> vfilter.HasFlag VisibilityFilter.Assembly

    let field (vfilter: VisibilityFilter) flags =
        match flags &&& FieldAttributes.FieldAccessMask with
        | FieldAttributes.Public -> vfilter.HasFlag VisibilityFilter.Public
        | FieldAttributes.Family -> vfilter.HasFlag VisibilityFilter.Family
        | FieldAttributes.Assembly -> vfilter.HasFlag VisibilityFilter.Assembly
        | FieldAttributes.FamANDAssem -> vfilter.HasFlag VisibilityFilter.FamAndAssem
        | FieldAttributes.FamORAssem -> vfilter.HasFlag VisibilityFilter.FamOrAssem
        | FieldAttributes.Private -> vfilter.HasFlag VisibilityFilter.Private
        | FieldAttributes.PrivateScope
        | _ -> vfilter.HasFlag VisibilityFilter.CompilerControlled

    let methodDef (vfilter: VisibilityFilter) flags =
        match flags &&& MethodAttributes.MemberAccessMask with
        | MethodAttributes.Public -> vfilter.HasFlag VisibilityFilter.Public
        | MethodAttributes.Family -> vfilter.HasFlag VisibilityFilter.Family
        | MethodAttributes.Assembly -> vfilter.HasFlag VisibilityFilter.Assembly
        | MethodAttributes.FamANDAssem -> vfilter.HasFlag VisibilityFilter.FamAndAssem
        | MethodAttributes.FamORAssem -> vfilter.HasFlag VisibilityFilter.FamOrAssem
        | MethodAttributes.Private -> vfilter.HasFlag VisibilityFilter.Private
        | MethodAttributes.PrivateScope
        | _ -> vfilter.HasFlag VisibilityFilter.CompilerControlled
