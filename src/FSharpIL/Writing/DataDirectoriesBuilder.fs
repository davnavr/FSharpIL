namespace FSharpIL.Writing

open System.Runtime.CompilerServices

open FSharpIL.PortableExecutable

[<Sealed>]
type DataDirectoriesBuilder internal () =
    [<DefaultValue>] val mutable ExportTable: RvaAndSize
    [<DefaultValue>] val mutable ImportTable: RvaAndSize
    [<DefaultValue>] val mutable ResourceTable: RvaAndSize
    [<DefaultValue>] val mutable ExceptionTable: RvaAndSize
    [<DefaultValue>] val mutable CertificateTable: RvaAndSize
    [<DefaultValue>] val mutable BaseRelocationTable: RvaAndSize
    [<DefaultValue>] val mutable DebugTable: RvaAndSize
    [<DefaultValue>] val mutable CopyrightTable: RvaAndSize
    [<DefaultValue>] val mutable GlobalPointerTable: RvaAndSize
    [<DefaultValue>] val mutable TLSTable: RvaAndSize
    [<DefaultValue>] val mutable LoadConfigTable: RvaAndSize
    [<DefaultValue>] val mutable BoundImportTable: RvaAndSize
    [<DefaultValue>] val mutable ImportAddressTable: RvaAndSize
    [<DefaultValue>] val mutable DelayImportDescriptor: RvaAndSize
    [<DefaultValue>] val mutable CliHeader: CliHeaderDirectory
    [<DefaultValue>] val mutable Reserved: RvaAndSize

    member this.ToImmutable() =
        { ExportTable = this.ExportTable
          ImportTable = this.ImportTable
          ResourceTable = this.ResourceTable
          ExceptionTable = this.ExceptionTable
          CertificateTable = this.CertificateTable
          BaseRelocationTable = this.BaseRelocationTable
          DebugTable = this.DebugTable
          CopyrightTable = this.CopyrightTable
          GlobalPointerTable = this.GlobalPointerTable
          TLSTable = this.TLSTable
          LoadConfigTable = this.LoadConfigTable
          BoundImportTable = this.BoundImportTable
          ImportAddressTable = this.ImportAddressTable
          DelayImportDescriptor = this.DelayImportDescriptor
          CliHeader = this.CliHeader
          Reserved = this.Reserved }
