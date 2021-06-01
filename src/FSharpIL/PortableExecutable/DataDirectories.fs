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

/// Represents the data directories of a PE file (II.25.2.3.3).
[<Sealed>]
type DataDirectories internal
    (
        exportTable: RvaAndSize,
        importTable: RvaAndSize,
        resourceTable: RvaAndSize,
        exceptionTable: RvaAndSize,
        certificateTable: RvaAndSize,
        baseRelocationTable: RvaAndSize,
        debugTable: RvaAndSize,
        copyrightTable: RvaAndSize,
        globalPointerTable: RvaAndSize,
        tlsTable: RvaAndSize,
        loadConfigTable: RvaAndSize,
        boundImportTable: RvaAndSize,
        importAddressTable: RvaAndSize,
        delayImportDescriptor: RvaAndSize,
        cliHeader: CliHeaderDirectory,
        reserved: RvaAndSize
    ) =
    member _.ExportTable = exportTable
    member _.ImportTable = importTable
    member _.ResourceTable = resourceTable
    member _.ExceptionTable = exceptionTable
    member _.CertificateTable = certificateTable
    member _.BaseRelocationTable = baseRelocationTable
    member _.DebugTable = debugTable
    member _.CopyrightTable = copyrightTable
    member _.GlobalPointerTable = globalPointerTable
    member _.TLSTable = tlsTable
    member _.LoadConfigTable = loadConfigTable
    member _.BoundImportTable = boundImportTable
    member _.ImportAddressTable = importAddressTable
    member _.DelayImportDescriptor = delayImportDescriptor
    member _.CliHeader = cliHeader
    member _.Reserved = reserved

[<RequireQualifiedAccess>]
module DataDirectories =
    let ofBuilder (builder: DataDirectoriesBuilder) =
        DataDirectories (
            builder.ExportTable,
            builder.ImportTable,
            builder.ResourceTable,
            builder.ExceptionTable,
            builder.CertificateTable,
            builder.BaseRelocationTable,
            builder.DebugTable,
            builder.CopyrightTable,
            builder.GlobalPointerTable,
            builder.TLSTable,
            builder.LoadConfigTable,
            builder.BoundImportTable,
            builder.ImportAddressTable,
            builder.DelayImportDescriptor,
            builder.CliHeader,
            builder.Reserved
        )
