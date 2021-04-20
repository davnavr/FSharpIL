[<RequireQualifiedAccess>]
module internal FSharpIL.Metadata.DelegateHelpers

open System.Collections.Immutable
open System.Reflection

// TODO: Does this correspond to 'runtime' keyword?
let implFlags = MethodImplAttributes.Managed ||| MethodImplAttributes.Runtime

let ctorFlags = ConstructorFlags(Public, true).Value ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName

let ctorSignature (builder: CliMetadataBuilder) =
    let parameters =
        [|
            EncodedType.Object
            EncodedType.I // native int
        |]
        |> Array.map ParamItem.create
        |> ImmutableArray.CreateRange
    let signature = MethodDefSignature(true, false, MethodCallingConventions.Default, ReturnType.itemVoid, parameters)
    builder.Blobs.MethodDefSig.GetOrAdd signature

let ctorParameters = ParamRows.addNamed [| "object"; "method" |]

let inline invokeSignature (builder: CliMetadataBuilder) (delegateDef: inref<DelegateDef>) =
    let signature = MethodDefSignature(true, false, Default, delegateDef.ReturnType, delegateDef.Parameters)
    builder.Blobs.MethodDefSig.GetOrAdd signature
