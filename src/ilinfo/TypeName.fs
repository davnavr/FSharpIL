﻿[<RequireQualifiedAccess>]
module ILInfo.TypeName

open System.Collections.Immutable
open System.IO

open FSharpIL.Reading

let private write name ns (strings: ParsedStringsStream) (wr: TextWriter) =
    wr.Write '''
    let ns' = strings.GetString ns
    if ns'.Length > 0 then
        wr.Write ns'
        wr.Write '.'
    wr.Write(strings.GetString name)
    wr.Write '''

let typeRef i (tables: ParsedMetadataTables) (strings: ParsedStringsStream) wr =
    match tables.TypeRef with
    | ValueNone -> fprintf wr "// TODO: Error when TypeRef table is missing while getting name of a TypeRef %i" i
    | ValueSome table ->
        match table.TryGetRow i with
        | Ok row ->
            wr.Write '['
            match row.ResolutionScope, tables.AssemblyRef with
            | ParsedResolutionScope.AssemblyRef i, ValueSome assem ->
                wr.Write(strings.GetString assem.[int32 i].Name)
            | _ -> fprintf wr "// TODO: Unsupported resolution scope %A" row.ResolutionScope
            wr.Write ']'
            write row.TypeName row.TypeNamespace strings wr
        | Error err -> fprintfn wr "// error : %s" err.Message // TODO: How to print error messages gracefully and without copying this over and over again?

let ofTypeDefOrRefOrSpec extends (tables: ParsedMetadataTables) strings wr =
    match extends with
    | ParsedTypeDefOrRefOrSpec.TypeDef i ->
        // Assume the TypeDef table exists if this function is being called, otherwise there wouldn't be a type to extend.
        let table = ValueOption.get tables.TypeDef
        match table.TryGetRow i with
        | Ok row ->
            let row = tables.TypeDef.Value.[int32 i]
            write row.TypeName row.TypeNamespace strings wr
        | Error err -> fprintfn wr "// error : %s" err.Message
    | ParsedTypeDefOrRefOrSpec.TypeRef i -> typeRef i tables strings wr
    | _ -> fprintf wr "// TODO: Handle incorrect type names %A" extends

/// Prints the custom modifiers of a type.
let cmodifiers (modifiers: ImmutableArray<ParsedCustomMod>) tables strings (wr: TextWriter) =
    for { ModifierRequired = req; ModifierType = mtype } in modifiers do
        wr.Write " mod"
        match req with
        | CustomModKind.OptionalModfier -> wr.Write "opt"
        | CustomModKind.RequiredModifier -> wr.Write "req"
        wr.Write '('
        ofTypeDefOrRefOrSpec mtype tables strings wr
        wr.Write ')'

let definedUserType kind t tables strings (wr: TextWriter) =
    match kind with
    | DefinedTypeKind.Class -> wr.Write "class "
    | DefinedTypeKind.ValueType -> wr.Write "valuetype "
    ofTypeDefOrRefOrSpec t tables strings wr

/// Prints a type (II.7.1).
let rec encoded (etype: ParsedType) tables strings wr =
    match etype with
    | ParsedType.Var i-> fprintf wr "!%i" i
    | ParsedType.MVar i -> fprintf wr "!!%i" i
    | ParsedType.Boolean -> wr.Write "bool"
    | ParsedType.Char -> wr.Write "char"
    | ParsedType.I1 -> wr.Write "int8"
    | ParsedType.U1 -> wr.Write "unsigned int8"
    | ParsedType.I2 -> wr.Write "int16"
    | ParsedType.U2 -> wr.Write "unsigned int16"
    | ParsedType.I4 -> wr.Write "int32"
    | ParsedType.U4 -> wr.Write "unsigned int32"
    | ParsedType.I8 -> wr.Write "int64"
    | ParsedType.U8 -> wr.Write "unsigned int64"
    | ParsedType.R4 -> wr.Write "float32"
    | ParsedType.R8 -> wr.Write "float64"
    | ParsedType.I -> wr.Write "native int"
    | ParsedType.U -> wr.Write "native unsigned int"
    | ParsedType.Object -> wr.Write "object"
    | ParsedType.String -> wr.Write "string"
    | ParsedType.Class t -> definedUserType DefinedTypeKind.Class t tables strings wr
    | ParsedType.ValueType t -> definedUserType DefinedTypeKind.ValueType t tables strings wr
    | ParsedType.SZArray(modifiers, t) ->
        encoded t tables strings wr
        wr.Write "[]"
        cmodifiers modifiers tables strings wr
    | ParsedType.Array(t, shape) ->
        encoded t tables strings wr
        wr.Write '['
        wr.Write "// TODO: Print array shape information"
        wr.Write ']'
    | ParsedType.GenericInst(kind, t, gargs) ->
        definedUserType kind t tables strings wr
        wr.Write '<'
        for i = 0 to gargs.Length - 1 do
            if i > 0 then wr.Write ", "
            encoded gargs.[i] tables strings wr
        wr.Write '>'
    | ParsedType.Ptr(modifiers, ptype) ->
        match ptype with
        | ValueNone -> wr.Write "void"
        | ValueSome t -> encoded t tables strings wr
        wr.Write '*'
        cmodifiers modifiers tables strings wr
