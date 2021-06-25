namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers internal (owner, warnings) =
    let mutable methods = HybridHashSet<DefinedMethod>()
    member _.MethodCount = methods.Count

[<Sealed>]
type ReferencedTypeMembers internal (warnings) = class end

type IndexLookup<'T, 'Row when 'Row :> ITableRow> = Dictionary<'T, TableIndex<'Row>>

[<Sealed>]
type ModuleBuilder
    (
        name,
        ?mvid,
        ?assembly: AssemblyDefinition,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?assemblyRefCapacity
    ) =
    let assemblyRefs = HashSet<AssemblyReference>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<DefinedType, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<ReferencedType, _>(defaultArg typeRefCapacity 32)

    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val Mvid = Option.defaultWith (fun() -> Guid.NewGuid()) mvid
    member _.Name: Identifier = name
    member _.Assembly = assembly
    member _.DefinedTypes = definedTypes.Keys :> IReadOnlyCollection<_>
    member _.ReferencedTypes = referencedTypes.Keys :> IReadOnlyCollection<_>
    member _.ReferencedAssemblies = assemblyRefs :> IReadOnlyCollection<_>

    member _.DefineType t =
        match definedTypes.TryGetValue t with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type def")
        | false, _ ->
            // TODO: Check that extends and nestedclass are already contained in the Module.
            let members = DefinedTypeMembers(t, warnings)
            definedTypes.[t] <- members
            Ok members

    member _.ReferenceAssembly assem =
        if not (assemblyRefs.Add assem) then
            () // TODO: Warning for duplicate assembly reference

    member this.Serialize() =
        let builder = CliMetadataBuilder(fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add this.Mvid))
        let assemblyRefLookup = IndexLookup<AssemblyReference, AssemblyRefRow> assemblyRefs.Count
        let definedTypeLookup = IndexLookup<DefinedType, TypeDefRow> definedTypes.Count

        let assemblyDef =
            match assembly with
            | Some assem ->
                let row =
                    { HashAlgId = AssemblyHashAlgorithm.SHA1 // TODO: Set the HashAlgId properly
                      MajorVersion = assem.Version.Major
                      MinorVersion = assem.Version.Minor
                      BuildNumber = assem.Version.Build
                      RevisionNumber = assem.Version.Revision
                      Flags = if assem.PublicKey.IsDefaultOrEmpty then AssemblyFlags.None else AssemblyFlags.PublicKey // TODO: Allow setting of other assembly flags
                      PublicKey = builder.Blob.Add assem.PublicKey
                      Name = builder.Strings.Add assem.Name
                      Culture = builder.Strings.Add assem.Culture }

                ValueSome(builder.Tables.Assembly.SetRow &row)
            | None -> ValueNone

        for assem in assemblyRefs do
            let row =
                { MajorVersion = assem.Version.Major
                  MinorVersion = assem.Version.Minor
                  BuildNumber = assem.Version.Build
                  RevisionNumber = assem.Version.Revision
                  PublicKeyOrToken = builder.Blob.Add assem.PublicKeyOrToken
                  Name = builder.Strings.Add assem.Name
                  Culture = builder.Strings.Add assem.Culture
                  HashValue = builder.Blob.Add assem.HashValue }

            assemblyRefLookup.[assem] <- builder.Tables.AssemblyRef.Add &row

        //for KeyValue(tdef, members) in definedTypes do
        //    let row =
        //        {  }

        //    definedTypeLookup.[tdef] <- builder.Tables.TypeDef.TryAddRow &row |> ValidationResult.get

        builder
