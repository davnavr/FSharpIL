[<RequireQualifiedAccess>]
module internal FSharpIL.XmlDoc.Name

open System.Xml

open FSharpIL.Metadata

let rec typeDef tdef (metadata: CliMetadata) (writer: XmlWriter): inref<_> =
    let tdef' = &metadata.TypeDef.[tdef]

    match tdef'.EnclosingClass with
    | ValueSome parent -> typeDef parent metadata writer |> ignore
    | ValueNone ->
        if tdef'.TypeNamespace.Length > 0 then
            writer.WriteRaw tdef'.TypeNamespace
            writer.WriteRaw "."

    writer.WriteRaw(tdef'.TypeName.ToString())
    &tdef'

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
        writer.WriteRaw(metadata.Field.[index'].Name.ToString())
    | DocMember.Property index' ->
        writer.WriteRaw "P:"
        failwith "TODO: Write name of property"
    | DocMember.Method index' ->
        writer.WriteRaw "M:"
        typeDef (metadata.MethodDef.GetOwner index') metadata writer |> ignore
        writer.WriteRaw "."
        writer.WriteRaw(metadata.MethodDef.[index'].Name.ToString())
    //| DocMember.Event index' ->
