[<AutoOpen>]
module FSharpIL.WritePE

open FSharpIL.Utilities

open FSharpIL.PETypes

type PEBuilder internal() =
    member _.Combine(header: PEFileHeader, pe: PortableExecutable) =
        { pe with PEFileHeader = header }
    member _.Delay(f): PortableExecutable = f()
    member _.Yield(header: PEFileHeader) = header
    member _.Zero(): PortableExecutable = PortableExecutable.Empty

/// Builds a Portable Executable file using a low-level computation expression syntax.
let pe = PEBuilder()

type AssemblyBuilder internal() =
    member _.Yield() = ()

/// Builds a .NET assembly using a high-level computation expression syntax.
let assembly = AssemblyBuilder()
