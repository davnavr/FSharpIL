[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildCli

open System.Collections.Generic

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers (owner: DefinedType) =
    [<DefaultValue>] val mutable Methods: HybridHashSet<DefinedMethod>

[<Sealed>]
type ReferencedTypeMembers (owner: ReferencedType) =
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

[<Struct>]
type MemberIndices =
    { mutable FieldList: TableIndex<FieldRow>
      mutable MethodList: TableIndex<MethodDefRow>
      mutable EventList: TableIndex<EventRow>
      mutable PropertyList: TableIndex<PropertyRow> }

let serialize
    header
    root
    (name: Identifier)
    mvid

    (assemblyReferences: HashSet<ReferencedAssembly>)
    (definedTypes: Dictionary<DefinedType, DefinedTypeMembers>)
    (moduleGlobalMembers: DefinedTypeMembers)
    (referencedTypes: Dictionary<ReferencedType, ReferencedTypeMembers>)
    =
    let builder =
        let strings = StringsStreamBuilder 1024
        let guids = GuidStreamBuilder 1
        let blobs = BlobStreamBuilder 512

        CliMetadataBuilder (
            header,
            root,
            FSharpIL.Writing.Cil.MethodBodyList(),
            MetadataTablesBuilder((fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add mvid)), strings, guids, blobs),
            strings,
            UserStringStreamBuilder 1,
            guids,
            blobs
        )

    let assemblyReferenceLookup = Dictionary<ReferencedAssembly, TableIndex<AssemblyRefRow>> assemblyReferences.Count
    let referencedTypeLookup = Dictionary<ReferencedType, _> referencedTypes.Count
    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count

    builder

type StatefulBuilder () =
    class end

let stateful = StatefulBuilder()

let metadata header root (name: Identifier) mvid builder state =
    let referencedAssemblies = HashSet<ReferencedAssembly> 4
    let definedTypes = Dictionary<DefinedType, _> 16
    let referencedTypes = Dictionary<ReferencedType, _> 32

    let inline warn (msg: IValidationWarning) =
        match builder.Warning with
        | Some warning -> warning state msg
        | None -> state

    let rec addDefinedType tdef =
        match definedTypes.TryGetValue tdef with
        | true, existing -> noImpl "TODO: Error for duplicate TypeDef"
        | false, _ ->
            definedTypes.[tdef] <- DefinedTypeMembers tdef

            //noImpl "TODO: Validation"

    and addReferencedType tref =
        match referencedTypes.TryGetValue tref with
        | true, existing -> noImpl "TODO: Error for duplicate TypeRef"
        | false, _ ->
            referencedTypes.[tref] <- ReferencedTypeMembers tref

            //noImpl "TODO: Validation"

    let rec inner state =
        match builder.Update state with
        | ModuleUpdate.DefineType tdef ->
            addDefinedType tdef
            inner (builder.DefineType state tdef)
        // TODO: How to define assembly? Maybe throw exception if called more than once, but that may be confusing and not user friendly.
        | ModuleUpdate.Finish ->
            let metadata = serialize header root name mvid referencedAssemblies definedTypes (noImpl "") referencedTypes
            metadata, state

    inner state
