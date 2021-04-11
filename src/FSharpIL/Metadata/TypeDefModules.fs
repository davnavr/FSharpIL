namespace FSharpIL.Metadata

open System.Reflection

[<RequireQualifiedAccess>]
module internal ClassDef =
    let inline tryAddRow builder (def: inref<ClassDef<_>>) =
        Unsafe.tryAddTypeDefRow<ClassDef<'Flags>>
            builder
            (def.Flags.Value ||| def.Access.Tag)
            def.ClassName
            def.TypeNamespace
            def.Extends
            (TypeVisibility.enclosingClass def.Access)

// TODO: Should inref be used for addRow functions for TypeDefs?

[<RequireQualifiedAccess>]
module ConcreteClass =
    let typeIndex (classDef: RawIndex<ConcreteClassDef>) = classDef.ChangeTag<TypeDefRow>()
    let inline tryAddRow builder (classDef: ConcreteClassDef): Result<RawIndex<ConcreteClassDef>, _> = ClassDef.tryAddRow builder &classDef
    let inline addRow builder classDef = tryAddRow builder classDef |> ValidationError.check

[<RequireQualifiedAccess>]
module StaticClass =
    let typeIndex (classDef: RawIndex<StaticClassDef>) = classDef.ChangeTag<TypeDefRow>()
    let inline tryAddRow builder (classDef: StaticClassDef): Result<RawIndex<StaticClassDef>, _> = ClassDef.tryAddRow builder &classDef
    let inline addRow builder classDef = tryAddRow builder classDef |> ValidationError.check

[<RequireQualifiedAccess>]
module Delegate =
    let typeIndex (delegateDef: RawIndex<DelegateDef>) = delegateDef.ChangeTag<TypeDefRow>()

//module Enum =

[<RequireQualifiedAccess>]
module Interface =
    let typeIndex (intfDef: RawIndex<InterfaceDef>) = intfDef.ChangeTag<TypeDefRow>()
    let inline tryAddRow builder (intfDef: InterfaceDef) =
        Unsafe.tryAddTypeDefRow<InterfaceDef>
            builder
            (intfDef.Access.Tag ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
            intfDef.InterfaceName
            intfDef.TypeNamespace
            Extends.Null
            (TypeVisibility.enclosingClass intfDef.Access)
    let inline addRow builder intfDef = tryAddRow builder intfDef |> ValidationError.check
