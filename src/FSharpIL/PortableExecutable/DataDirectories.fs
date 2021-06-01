namespace FSharpIL.PortableExecutable

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CliHeaderDirectory = internal { CliHeader: RvaAndSize }
// TODO: Add other special types for other data directories.

[<AutoOpen>]
module DataDirectory =
    let (|CliHeader|) { CliHeader = data } = data

[<NoComparison; StructuralEquality>]
type DataDirectoriesBuilder =
    { [<DefaultValue>] mutable ExportTable: RvaAndSize
      mutable ImportTable: RvaAndSize
      [<DefaultValue>] mutable ResourceTable: RvaAndSize
      [<DefaultValue>] mutable ExceptionTable: RvaAndSize
      [<DefaultValue>] mutable CertificateTable: RvaAndSize
      [<DefaultValue>] mutable BaseRelocationTable: RvaAndSize
      [<DefaultValue>] mutable DebugTable: RvaAndSize
      [<DefaultValue>] mutable CopyrightTable: RvaAndSize
      [<DefaultValue>] mutable GlobalPointerTable: RvaAndSize
      [<DefaultValue>] mutable TLSTable: RvaAndSize
      [<DefaultValue>] mutable LoadConfigTable: RvaAndSize
      [<DefaultValue>] mutable BoundImportTable: RvaAndSize
      mutable ImportAddressTable: RvaAndSize
      [<DefaultValue>] mutable DelayImportDescriptor: RvaAndSize
      mutable CliHeader: CliHeaderDirectory
      [<DefaultValue>] mutable Reserved: RvaAndSize }
