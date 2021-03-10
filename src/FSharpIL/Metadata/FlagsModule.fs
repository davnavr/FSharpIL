﻿[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Flags

open System.Reflection

let concreteClass (Flags flags: ClassFlags) = TypeFlags<ConcreteClassFlags> flags
let abstractClass (Flags flags: ClassFlags) = TypeFlags<AbstractClassFlags>(TypeAttributes.Abstract ||| flags)
let sealedClass (Flags flags: ClassFlags) = TypeFlags<SealedClassFlags>(TypeAttributes.Sealed ||| flags)
let staticClass (Flags flags: ClassFlags) = TypeFlags<StaticClassFlags>(TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| flags)
let valueType (Flags flags: ClassFlags) = TypeFlags<StructFlags>(TypeAttributes.Sealed ||| flags)

let instanceField (Flags flags: FieldFlags<Visibility>) = ValidFlags<InstanceField, _> flags
let staticField (Flags flags: FieldFlags<Visibility>) = ValidFlags<StaticField, _>(flags ||| FieldAttributes.Static)
let globalField (Flags flags: FieldFlags<GlobalVisibility>) = ValidFlags<GlobalField, _>(flags ||| FieldAttributes.Static) // TODO: Set other special flags

let instanceMethod (Flags flags: InstanceMethodFlags) = ValidFlags<InstanceMethod, _> flags
let abstractMethod (Flags flags: InstanceMethodFlags) = ValidFlags<AbstractMethod, _>(MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| flags)
let finalMethod (Flags flags: InstanceMethodFlags) = ValidFlags<FinalMethod, _>(MethodAttributes.Final ||| MethodAttributes.Virtual ||| flags)
let staticMethod (Flags flags: StaticMethodFlags<Visibility>) = ValidFlags<StaticMethod, _>(MethodAttributes.Static ||| flags)
// let globalMethod (Flags flags: StaticMethodDefFlags<GlobalVisibility>) = failwith ""

let constructor (Flags flags: ConstructorFlags) = ValidFlags<ObjectConstructor, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| flags)
let classConstructor (Flags flags: ConstructorFlags) = ValidFlags<ClassConstructor, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.Static ||| flags)

/// Flags for non-variant generic parameters.
let invariant (Flags flags: GenericParamFlags) = ValidFlags<InvariantGenericParamFlags, _> flags
/// Flags for generic parameters that can be covariant.
let covariant (covariant: bool) (Flags flags: GenericParamFlags) =
    if covariant
    then GenericParameterAttributes.Covariant ||| flags
    else flags
    |> ValidFlags<CovariantGenericParamFlags, _>
/// Flags fo generic parameters that can be contravariant.
let contravariant (contravariant: bool) (Flags flags: GenericParamFlags) =
    if contravariant
    then GenericParameterAttributes.Contravariant ||| flags
    else flags
    |> ValidFlags<ContravariantGenericParamFlags, _>
