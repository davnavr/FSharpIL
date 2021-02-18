[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Flags

open System.Reflection

let concreteClassFlags (Flags flags: ClassFlags) = TypeFlags<ConcreteClassFlags> flags
let abstractClassFlags (Flags flags: ClassFlags) = TypeFlags<AbstractClassFlags>(TypeAttributes.Abstract ||| flags)
let sealedClassFlags (Flags flags: ClassFlags) = TypeFlags<SealedClassFlags>(TypeAttributes.Sealed ||| flags)
let staticClassFlags (Flags flags: ClassFlags) = TypeFlags<StaticClassFlags>(TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| flags)
let structFlags (Flags flags: ClassFlags) = TypeFlags<StructFlags>(TypeAttributes.Sealed ||| flags)

let instanceFieldFlags (Flags flags: FieldFlags<Visibility>) = ValidFlags<InstanceFieldFlags, _> flags
let staticFieldFlags (Flags flags: FieldFlags<Visibility>) = ValidFlags<StaticFieldFlags, _>(flags ||| FieldAttributes.Static)
let globalFieldFlags (Flags flags: FieldFlags<GlobalVisibility>) = ValidFlags<GlobalFieldFlags, _>(flags ||| FieldAttributes.Static) // TODO: Set other special flags

let instanceMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<InstanceMethodFlags, _> flags
let abstractMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<AbstractMethodFlags, _>(MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| flags)
let finalMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<FinalMethodFlags, _>(MethodAttributes.Final ||| MethodAttributes.Virtual ||| flags)
let staticMethodFlags (Flags flags: StaticMethodDefFlags<Visibility>) = ValidFlags<StaticMethodFlags, _>(MethodAttributes.Static ||| flags)
// let globalMethodFlags (Flags flags: StaticMethodDefFlags<GlobalVisibility>) = invalidOp ""

// let constructorFlags (Flags flags: _) = ValidFlags<ConstructorFlags, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| flags)
// let classConstructorFlags (Flags flags: _) = ValidFlags<ClassConstructorFlags, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.Static ||| flags)

/// Flags for non-variant generic parameters.
let invariantFlags (Flags flags: GenericParamFlags) = ValidFlags<NonVariantGenericParamFlags, _> flags
/// Flags for generic parameters that can be covariant.
let covariantFlags (covariant: bool) (Flags flags: GenericParamFlags) =
    if covariant
    then GenericParameterAttributes.Covariant ||| flags
    else flags
    |> ValidFlags<CovariantGenericParamFlags, _>
/// Flags fo generic parameters that can be contravariant.
let contravariantFlags (contravariant: bool) (Flags flags: GenericParamFlags) =
    if contravariant
    then GenericParameterAttributes.Contravariant ||| flags
    else flags
    |> ValidFlags<ContravariantGenericParamFlags, _>
