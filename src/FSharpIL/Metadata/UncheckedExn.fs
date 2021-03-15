/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings, throwing an exception on any errors.
/// </summary>
module FSharpIL.Metadata.UncheckedExn

open System

let private throwOnError =
    function
    | Ok item -> item
    | Error err -> raise(ValidationErrorException err)

[<AbstractClass; Sealed>]
type Unsafe = class
    static member AddStruct(builder, valueType, structDef) =
        Unchecked.Unsafe.AddStruct(builder, valueType, structDef) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent): TypeDefIndex<'Tag> =
        Unchecked.Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends) =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, None)
end

let addStruct builder: TypeDefIndex<StructDef> = failwith "TODO: How to add struct?"

let referenceType builder typeRef = Unchecked.referenceType builder typeRef |> throwOnError
