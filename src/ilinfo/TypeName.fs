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

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type Encoded<'Writer when 'Writer :> TextWriter> (wr: 'Writer) =
    interface IEncodedTypeParser with
        member _.Class(arg1: ParsedTypeDefOrRefOrSpec): unit = ()
        member _.MVar i = fprintf wr "!!%i" i
        member _.Primitive tag =
            match tag with
            | ElementType.Boolean -> wr.Write "bool"
            | ElementType.Char -> wr.Write "char"
            | ElementType.I1 -> wr.Write "int8"
            | ElementType.U1 -> wr.Write "unsigned int8"
            | ElementType.I2 -> wr.Write "int16"
            | ElementType.U2 -> wr.Write "unsigned int16"
            | ElementType.I4 -> wr.Write "int32"
            | ElementType.U4 -> wr.Write "unsigned int32"
            | ElementType.I8 -> wr.Write "int64"
            | ElementType.U8 -> wr.Write "unsigned int64"
            | ElementType.R4 -> wr.Write "float32"
            | ElementType.R8 -> wr.Write "float64"
            | ElementType.I -> wr.Write "native int"
            | ElementType.U -> wr.Write "native unsigned int"
            | ElementType.Object -> wr.Write "object"
            | ElementType.String -> wr.Write "string"
            | bad -> fprintf wr "TODO: Add support for \"%A\"" bad
        member this.ValueType(arg1: ParsedTypeDefOrRefOrSpec): unit = ()
        member this.Var i = fprintf wr "!%i" i
