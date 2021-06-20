namespace FSharpIL.PortableExecutable

open System.Runtime.CompilerServices

type [<IsReadOnly; Struct; RequireQualifiedAccess>] CliHeaderDirectory = internal { Directory: RvaAndSize }
// TODO: Add other special types for other data directories.

[<RequireQualifiedAccess>]
module DataDirectory =
    let inline (|CliHeader|) { CliHeaderDirectory.Directory = data } = data

/// Represents the data directories of a PE file (II.25.2.3.3).
[<NoComparison; NoEquality>]
type DataDirectories =
    { ExportTable: RvaAndSize
      ImportTable: RvaAndSize
      ResourceTable: RvaAndSize
      ExceptionTable: RvaAndSize
      CertificateTable: RvaAndSize
      BaseRelocationTable: RvaAndSize
      DebugTable: RvaAndSize
      CopyrightTable: RvaAndSize
      GlobalPointerTable: RvaAndSize
      TLSTable: RvaAndSize
      LoadConfigTable: RvaAndSize
      BoundImportTable: RvaAndSize
      ImportAddressTable: RvaAndSize
      DelayImportDescriptor: RvaAndSize
      CliHeader: CliHeaderDirectory
      Reserved: RvaAndSize }
