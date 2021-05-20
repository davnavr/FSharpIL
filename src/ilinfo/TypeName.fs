[<RequireQualifiedAccess>]
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
        let row = table.[int32 i]
        wr.Write '['
        match row.ResolutionScope, tables.AssemblyRef with
        | ParsedResolutionScope.AssemblyRef i, ValueSome assem ->
            wr.Write(strings.GetString assem.[int32 i].Name)
        | _ -> fprintf wr "// TODO: Unsupported resolution scope %A" row.ResolutionScope
        wr.Write ']'
        write row.TypeName row.TypeNamespace strings wr

let ofTypeDefOrRefOrSpec extends (tables: ParsedMetadataTables) strings wr =
    match extends with
    | ParsedTypeDefOrRefOrSpec.TypeDef i ->
        // Assume the TypeDef table exists if this function is being called, otherwise there wouldn't be a type to extend.
        let row = tables.TypeDef.Value.[int32 i]
        write row.TypeName row.TypeNamespace strings wr
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
    | ParsedType.Class t -> fprintf wr "class %t" (ofTypeDefOrRefOrSpec t tables strings)
    | ParsedType.ValueType t -> fprintf wr "valuetype %t" (ofTypeDefOrRefOrSpec t tables strings)
    | ParsedType.SZArray(modifiers, t) ->
        encoded t tables strings wr
        cmodifiers modifiers tables strings wr
        wr.Write "[]"
