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
    static member AddStruct(builder, valueType: RawIndex<TypeRef>, structDef): RawIndex<_> =
        Unchecked.Unsafe.AddStruct(builder, valueType, structDef) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent): RawIndex<'Tag> =
        Unchecked.Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, typeNamespace, extends, parent) |> throwOnError

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends): RawIndex<_> =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, ValueNone)

    static member AddInstanceProperty(builder, parent, property, getter, setter) =
        Unchecked.Unsafe.AddInstanceProperty(builder, parent, property, getter = getter, setter = setter) |> throwOnError
end

[<RequireQualifiedAccess>]
module ConcreteClass =
    let addTypeDef builder classDef = Unchecked.ConcreteClass.addTypeDef builder classDef |> throwOnError
    let addInstanceMethod builder owner method = Unchecked.ConcreteClass.addInstanceMethod builder owner method |> throwOnError
    let addConstructor builder owner method = Unchecked.ConcreteClass.addConstructor builder owner method |> throwOnError
    let addInstanceField builder owner field = Unchecked.ConcreteClass.addInstanceField builder owner field |> throwOnError

[<RequireQualifiedAccess>]
module StaticClass =
    let addTypeDef builder classDef = Unchecked.StaticClass.addTypeDef builder classDef |> throwOnError
    let addEntryPoint builder owner method = Unchecked.StaticClass.addEntryPoint builder owner method |> throwOnError

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

let referenceAssembly builder assembly = Unchecked.referenceAssembly builder assembly

let addTypeSpec builder typeSpec = Unchecked.addTypeSpec builder typeSpec |> throwOnError

[<RequireQualifiedAccess>]
module GenericParam =
    let addNonvariant builder flags owner name constraints =
        Unchecked.GenericParam.addNonvariant builder flags owner name constraints |> throwOnError
