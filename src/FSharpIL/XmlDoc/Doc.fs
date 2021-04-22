[<RequireQualifiedAccess>]
module FSharpIL.XmlDoc.Doc

open System.Xml

open FSharpIL.Metadata

let private name index metadata (writer: XmlWriter) =
    writer.WriteStartAttribute "name"
    Name.write index metadata writer
    writer.WriteEndAttribute()

let cref mber metadata (writer: XmlWriter) =
    writer.WriteStartAttribute "cref"
    Name.write mber metadata writer
    writer.WriteEndAttribute()

let see mber metadata (writer: XmlWriter) =
    writer.WriteStartElement "see"
    cref mber metadata writer
    writer.WriteEndElement()

let generate generator (writer: XmlWriter) metadata (assemblyName: AssemblyName) (members: seq<DocMember>) =
    writer.WriteStartElement "doc"
    writer.WriteElementString("assembly", assemblyName.ToString())
    writer.WriteStartElement "members"
    writer.WriteEndElement() // members

    for mber in members do
        writer.WriteStartElement "member"
        name mber metadata writer
        generator mber writer
        writer.WriteEndElement() // member

    writer.WriteEndElement() // doc

let generateDoc generator metadata (writer: XmlWriter) assemblyName members =
    writer.WriteStartDocument()
    generate generator writer metadata assemblyName members
    writer.WriteEndDocument()
