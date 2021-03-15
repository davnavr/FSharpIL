/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings, throwing an exception on any errors.
/// </summary>
module FSharpIL.Metadata.UncheckedExn

open System

let private throwOnError: Result<_, ValidationError> -> _ =
    function
    | Ok item -> item
    | Error err -> raise(ValidationErrorException err)

[<AbstractClass; Sealed>]
type Unsafe = class
    static member AddStruct(builder, valueType: SimpleIndex<TypeRef>, structDef): TypeDefIndex<_> =
        Unchecked.Unsafe.AddStruct(builder, valueType, structDef) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent): TypeDefIndex<'Tag> =
        Unchecked.Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends): TypeDefIndex<_> =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, None)
end

[<RequireQualifiedAccess>]
module Struct =
    let addTypeDef builder lookup structDef = Unchecked.Struct.addTypeDef builder lookup structDef |> throwOnError
    let addInstanceMethod builder owner method = Unchecked.Struct.addInstanceMethod builder owner method |> throwOnError
    let addStaticMethod builder owner method = Unchecked.Struct.addStaticMethod builder owner method |> throwOnError
    let addConstructor builder owner method = Unchecked.Struct.addConstructor builder owner method |> throwOnError
    let addClassConstructor builder owner method = Unchecked.Struct.addClassConstructor builder owner method |> throwOnError
    let addEntryPoint builder owner method = Unchecked.Struct.addEntryPoint builder owner method |> throwOnError
    let addInstanceField builder owner field = Unchecked.Struct.addInstanceField builder owner field |> throwOnError
    let addStaticField builder owner field = Unchecked.Struct.addStaticField builder owner field |> throwOnError

let referenceType builder typeRef = Unchecked.referenceType builder typeRef |> throwOnError
