namespace FSharpIL.Metadata

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
    let tryAddRow builder (classDef: ConcreteClassDef): Result<RawIndex<ConcreteClassDef>, _> = ClassDef.tryAddRow builder &classDef
    let inline addRow builder classDef = tryAddRow builder classDef |> ValidationError.check

[<RequireQualifiedAccess>]
module StaticClass =
    let tryAddRow builder (classDef: StaticClassDef): Result<RawIndex<StaticClassDef>, _> = ClassDef.tryAddRow builder &classDef
    let inline addRow builder classDef = tryAddRow builder classDef |> ValidationError.check
