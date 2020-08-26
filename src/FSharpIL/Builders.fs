[<AutoOpen>]
module FSharpIL.Builders

/// Builds a Portable Executable file using a low-level computation expression syntax.
let pe = WritePE.Builder()

/// Builds a .NET assembly using a high-level computation expression syntax.
let assembly = invalidOp "no impl": obj
