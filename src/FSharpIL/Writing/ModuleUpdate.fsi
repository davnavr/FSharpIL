namespace FSharpIL.Writing

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type ModuleUpdate =
    internal
    | DefineAssembly of DefinedAssembly
    | DefineType of DefinedType
    | Finish

module ModuleUpdate =
    val Finish : ModuleUpdate

    //val Unsafe : (CliMetadataBuilder -> unit) -> ModuleUpdate

    val DefineAssembly : assembly: DefinedAssembly -> ModuleUpdate

    [<RequireQualifiedAccess>]
    module DefineType =
        val unsafe :
            flags: TypeDefFlags ->
            extends: ClassExtends ->
            typeNamespace: Identifier voption ->
            enclosingType: DefinedType voption ->
            typeName: Identifier ->
            ModuleUpdate

        // TODO: Bring back TypeKinds
