[<RequireQualifiedAccess>]
module internal FSharpIL.Metadata.DelegateHelpers

open System.Collections.Immutable
open System.Reflection

let implFlags = MethodImplAttributes.Managed ||| MethodImplAttributes.Runtime

let ctorFlags = ConstructorFlags(Public, true).Value ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName

let ctorSignature =
    let parameters = ImmutableArray.Create(ParamItem.create EncodedType.Object, ParamItem.create EncodedType.I)
    let signature = MethodDefSignature(true, false, MethodCallingConventions.Default, ReturnType.itemVoid, parameters)
    fun (builder: CliMetadataBuilder) -> builder.Blobs.MethodDefSig.GetOrAdd signature

let ctorParameters = ParamRows.addNamed [| "object"; "method" |]

let endInvokeParamRows = ParamRows.addNamed [| "result" |]
