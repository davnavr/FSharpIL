[<RequireQualifiedAccess>]
module internal FSharpIL.XmlDoc.Name

open System.Collections.Immutable
open System.Xml

open FSharpIL.Metadata

let identifier (Identifier name) (writer: XmlWriter) = writer.WriteRaw(name.Replace('.', '#'))

let rec typeDef tdef (metadata: CliMetadata) (writer: XmlWriter): inref<_> =
    let tdef' = &metadata.TypeDef.[tdef]

    match tdef'.EnclosingClass with
    | ValueSome parent -> typeDef parent metadata writer |> ignore
    | ValueNone ->
        if tdef'.TypeNamespace.Length > 0 then
            writer.WriteRaw tdef'.TypeNamespace
            writer.WriteRaw "."

    identifier tdef'.TypeName writer
    &tdef'

let private regularType (metadata: CliMetadata) (writer: XmlWriter) =
    function
    | TypeDefOrRefOrSpecEncoded.TypeDef tdef ->
        typeDef tdef metadata writer |> ignore
        ()

let private cmodifiers (modifiers: ImmutableArray<CustomModifier>) metadata (writer: XmlWriter) =
    for cmod in modifiers do
        writer.WriteRaw(if cmod.Required then "|" else "!")
        regularType metadata writer cmod.ModifierType

let rec encodedType (metadata: CliMetadata) (writer: XmlWriter) =
    function
    | EncodedType.Boolean -> writer.WriteRaw "System.Boolean"
    | EncodedType.Char -> writer.WriteRaw "System.Char"
    | EncodedType.I1 -> writer.WriteRaw "System.SByte"
    | EncodedType.U1 -> writer.WriteRaw "System.Byte"
    | EncodedType.I2 -> writer.WriteRaw "System.Int16"
    | EncodedType.U2 -> writer.WriteRaw "System.UInt16"
    | EncodedType.I4 -> writer.WriteRaw "System.Int32"
    | EncodedType.U4 -> writer.WriteRaw "System.UInt32"
    | EncodedType.I8 -> writer.WriteRaw "System.Int64"
    | EncodedType.U8 -> writer.WriteRaw "System.UInt64"
    | EncodedType.R4 -> writer.WriteRaw "System.Single"
    | EncodedType.R8 -> writer.WriteRaw "System.Double"
    | EncodedType.I -> writer.WriteRaw "System.IntPtr"
    | EncodedType.U -> writer.WriteRaw "System.UIntPtr"
    //| EncodedType.Array(
    | EncodedType.Class t
    | EncodedType.ValueType t -> regularType metadata writer t
    //| EncodedType.FunctionPointer(

    | EncodedType.MVar i -> writer.WriteRaw(sprintf "`%i" i)
    | EncodedType.Object -> writer.WriteRaw "System.Object"

    | EncodedType.String -> writer.WriteRaw "System.String"
    | EncodedType.SZArray(modifiers, elem) ->
        encodedType metadata writer elem
        cmodifiers modifiers metadata writer
        writer.WriteRaw "[]"
    //| EncodedType.Var i ->
    // TODO: Figure out how Var is used.

let write index (metadata: CliMetadata) (writer: XmlWriter) =
    match index with
    | DocMember.Type index' ->
        writer.WriteRaw "T:"
        let tdef = typeDef index' metadata writer
        () // TODO: Write generic parameters of type
    | DocMember.Field index' ->
        writer.WriteRaw "F:"
        typeDef (metadata.Field.GetOwner index') metadata writer |> ignore
        // TODO: Write generic parameters of type
        writer.WriteRaw "."
        identifier metadata.Field.[index'].Name writer
    | DocMember.Property index' ->
        writer.WriteRaw "P:"
        failwith "TODO: Write name of property"
    | DocMember.Method index' ->
        writer.WriteRaw "M:"
        typeDef (metadata.MethodDef.GetOwner index') metadata writer |> ignore
        writer.WriteRaw "."

        let method = &metadata.MethodDef.[index']
        identifier method.Name writer

        let signature = &metadata.Blobs.MethodDefSig.ItemRef method.Signature
        let parameters = signature.Parameters

        for i = 1 to parameters.Length do
            if i = 1 then writer.WriteRaw "("
            encodedType metadata writer parameters.[i - 1].ParamType
            if i < parameters.Length
            then writer.WriteRaw ","
            else writer.WriteRaw ")"

        // TODO: Check if method is an operator.
        if false then
            writer.WriteRaw "~"
            match signature.ReturnType.ReturnType with
            | ReturnType.Type t -> encodedType metadata writer t
            | bad -> failwithf "Cannot write unsupported method return type %A for signature in XML documentation" bad
    //| DocMember.Event index' ->
