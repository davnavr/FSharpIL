module FSharpIL.Cli.Signatures

open FSharpIL.Metadata.Signatures

type CustomMod = CustomMod<TypeDefOrRefOrSpec>
type CustomModifiers = CustomModifiers<TypeDefOrRefOrSpec>
type EncodedType = EncodedType<Type, TypeDefOrRefOrSpec>
type FieldSig = FieldSig<Type, TypeDefOrRefOrSpec>
type ParamItem = ParamItem<Type, TypeDefOrRefOrSpec>
type ReturnType = ReturnType<Type, TypeDefOrRefOrSpec>
type MethodDefSig = MethodDefSig<Type, TypeDefOrRefOrSpec>
type MethodRefSig = MethodRefSig<Type, TypeDefOrRefOrSpec>
type PropertySig = PropertySig<Type, TypeDefOrRefOrSpec>
type GenericArgList = GenericArgList<Type, TypeDefOrRefOrSpec>
type GenericInst = GenericInst<Type, TypeDefOrRefOrSpec>
type Pointer = Pointer<Type, TypeDefOrRefOrSpec>
type LocalVarSig = LocalVarSig<Type, TypeDefOrRefOrSpec>
