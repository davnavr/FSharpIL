// TODO: Replace ValidFlags type, since it is annoying to call these functions even though most classes, fields, and methods only use one flag type anyway.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Flags

open System.Reflection

let concreteClass (Flags flags: ClassFlags) = TypeFlags<ConcreteClassTag> flags
let abstractClass (Flags flags: ClassFlags) = TypeFlags<AbstractClassTag>(TypeAttributes.Abstract ||| flags)
let sealedClass (Flags flags: ClassFlags) = TypeFlags<SealedClassTag>(TypeAttributes.Sealed ||| flags)
let staticClass (Flags flags: ClassFlags) = TypeFlags<StaticClassTag>(TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| flags)
let valueType (Flags flags: ClassFlags) = TypeFlags<StructFlags>(TypeAttributes.Sealed ||| flags)

let instanceField (Flags flags: FieldFlags<Visibility>) = ValidFlags<InstanceFieldTag, _> flags
let staticField (Flags flags: FieldFlags<Visibility>) = ValidFlags<StaticFieldTag, _>(flags ||| FieldAttributes.Static)
let globalField (Flags flags: FieldFlags<GlobalVisibility>) = ValidFlags<GlobalField, _>(flags ||| FieldAttributes.Static) // TODO: Set other special flags

let instanceMethod (Flags flags: InstanceMethodFlags) = ValidFlags<InstanceMethodTag, _> flags
let abstractMethod (Flags flags: InstanceMethodFlags) = ValidFlags<AbstractMethodTag, _>(MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| flags)
let finalMethod (Flags flags: InstanceMethodFlags) = ValidFlags<FinalMethodTag, _>(MethodAttributes.Final ||| MethodAttributes.Virtual ||| flags)
let staticMethod (Flags flags: StaticMethodFlags<Visibility>) = ValidFlags<StaticMethodTag, _>(MethodAttributes.Static ||| flags)
// let globalMethod (Flags flags: StaticMethodDefFlags<GlobalVisibility>) = failwith ""

let constructor (Flags flags: ConstructorFlags) = ValidFlags<ObjectConstructorTag, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| flags)
let classConstructor (Flags flags: ConstructorFlags) = ValidFlags<ClassConstructorTag, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.Static ||| flags)

/// Flags for non-variant generic parameters.
[<System.Obsolete>]
let invariant (Flags flags: GenericParamFlags) = ValidFlags<InvariantGenericParamFlags, _> flags
/// Flags for generic parameters that can be covariant.
[<System.Obsolete>]
let covariant (covariant: bool) (Flags flags: GenericParamFlags) =
    if covariant
    then GenericParameterAttributes.Covariant ||| flags
    else flags
    |> ValidFlags<CovariantGenericParamFlags, _>
/// Flags fo generic parameters that can be contravariant.
[<System.Obsolete>]
let contravariant (contravariant: bool) (Flags flags: GenericParamFlags) =
    if contravariant
    then GenericParameterAttributes.Contravariant ||| flags
    else flags
    |> ValidFlags<ContravariantGenericParamFlags, _>
