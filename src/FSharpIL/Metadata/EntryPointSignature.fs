[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.EntryPointSignature

open System.Collections.Immutable

let inline private create (signature: MethodDefSignature) =
    let signature' = signature
    { new EntryPointSignature() with
        member _.CheckOwner owner = signature'.CheckOwner owner
        member _.Signature() = signature' }

let private args = EncodedType.SZArray(ImmutableArray.Empty, EncodedType.String) |> ParamItem.create |> ImmutableArray.Create
/// Represents the signature of an entrypoint method that takes an array of string arguments and returns an integer exit code.
let exitWithArgs = MethodDefSignature(false, false, MethodCallingConventions.Default, ReturnType.itemI4, args) |> create
/// Represents the signature of an entrypoint method that takes an array of string arguments and returns nothing.
let voidWithArgs = MethodDefSignature(false, false, MethodCallingConventions.Default, ReturnType.itemVoid, args) |> create
/// Represents the signature of an entrypoint method that takes no arguments and returns an integer exit code.
let exitNoArgs = MethodDefSignature(false, false, MethodCallingConventions.Default, ReturnType.itemI4, ImmutableArray.Empty)
/// Represents the signature of an entrypoint method that takes no arguments and returns nothing.
let voidNoArgs = MethodDefSignature(false, false, MethodCallingConventions.Default, ReturnType.itemVoid, ImmutableArray.Empty)
