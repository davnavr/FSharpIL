namespace FSharpIL.XmlDoc

open System.Runtime.CompilerServices
open System.Xml

open FSharpIL.Metadata

[<IsReadOnly; IsByRefLike; Struct>]
type AssemblyDocumentation = private AssemblyDocumentation of XmlWriter

type MemberDocumentation = private MemberDocumentation of XmlWriter

[<RequireQualifiedAccess>]
module Documentation =
    let writeRoot (writer: XmlWriter): AssemblyDocumentation =
        writer.WriteStartElement "doc"
        AssemblyDocumentation writer
    let writeAssemblyName (name: AssemblyName) (AssemblyDocumentation writer) =
        writer.WriteElementString("assembly", name.ToString())
        writer.WriteEndElement() // doc
        writer.WriteStartElement "members"
        MemberDocumentation writer
    let writeEnd (MemberDocumentation writer) =
        writer.WriteEndElement() // members
        writer.WriteEndElement() // doc
