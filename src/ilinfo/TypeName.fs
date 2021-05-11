[<RequireQualifiedAccess>]
module ILInfo.TypeName

open System.IO

open FSharpIL.Metadata
open FSharpIL.Reading

let private write name ns (strings: ParsedStringsStream) (wr: TextWriter) =
    wr.Write '''
    let ns' = strings.GetString ns
    if ns'.Length > 0 then
        wr.Write ns'
        wr.Write '.'
    wr.Write(strings.GetString name)
    wr.WriteLine '''

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

let encoded etype wr =
    match etype with
    | EncodedType.Var i -> fprintfn wr "!%i" i
    | EncodedType.MVar i -> fprintfn wr "!!%i" i
    | EncodedType.Boolean -> wr.Write "bool"
    | EncodedType.Char -> wr.Write "char"
    | EncodedType.I1 -> wr.Write "int8"
    | EncodedType.U1 -> wr.Write "unsigned int8"
    | EncodedType.I2 -> wr.Write "int16"
    | EncodedType.U2 -> wr.Write "unsigned int16"
    | EncodedType.I4 -> wr.Write "int32"
    | EncodedType.U4 -> wr.Write "unsigned int32"
    | EncodedType.I8 -> wr.Write "int64"
    | EncodedType.U8 -> wr.Write "unsigned int64"
    | EncodedType.R4 -> wr.Write "float32"
    | EncodedType.R8 -> wr.Write "float64"
    | EncodedType.I -> wr.Write "native int"
    | EncodedType.U -> wr.Write "native unsigned int"
    | EncodedType.Object -> wr.Write "object"
    | EncodedType.String -> wr.Write "string"
    | bad -> fprintf wr "TODO: Add support for \"%A\"" bad
