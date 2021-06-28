namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers =
    val private owner: DefinedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    
    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.MethodCount = this.Method.Count

    member this.AddMethod(method: DefinedMethod) = // TODO: return something like a struct(DefinedType * DefinedMethod)
        match this.owner with
        //| DefinedType.Enum _ -> noImpl "error for enum cannot have methods"
        //| DefinedType.Delegate _ -> noImpl "error for delegate cannot have additional methods maybe go check if ECMA-335 prohibits this in the list of rules, since section I says so"
        | :? TypeDefinition<TypeKinds.StaticClass> ->
            match method with
            | :? MethodDefinition<MethodKinds.Static> ->
                if this.Method.Add method
                then ValidationResult.Ok()
                else noImpl "error for duplicate method"
            | _ -> noImpl "bad"
        | _ -> noImpl "bad"

[<Sealed>]
type ReferencedTypeMembers = class
    val private owner: ReferencedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.MethodCount = this.Method.Count
end

[<IsReadOnly; Struct>]
type ReferencedTypeEntry =
    { TypeRef: TableIndex<TypeRefRow>
      Members: Dictionary<obj, TableIndex<MemberRefRow>> }

[<NoComparison; NoEquality>]
type DefinedTypeEntry =
    { TypeDef: TableIndex<TypeDefRow>
      Methods: Dictionary<DefinedMethod, TableIndex<MethodDefRow>> }

type MemberIndices () =
    [<DefaultValue>] val mutable FieldList: TableIndex<FieldRow>
    [<DefaultValue>] val mutable MethodList: TableIndex<MethodDefRow>
    [<DefaultValue>] val mutable EventList: TableIndex<EventRow>
    [<DefaultValue>] val mutable PropertyList: TableIndex<PropertyRow>

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
    static let moduleTypeName = Identifier.ofStr "<Module>"

    let assemblyRefs = HashSet<AssemblyReference>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<DefinedType, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<ReferencedType, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)

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

    member _.ReferenceType t =
        match referencedTypes.TryGetValue t with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type ref")
        | false, _ ->
            // TODO: Check that the resolution scope is already accounted for.
            let members = ReferencedTypeMembers(t, warnings)
            referencedTypes.[t] <- members
            Ok members

    member _.ReferenceAssembly assem =
        if not (assemblyRefs.Add assem) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member private this.SerializeReferencedType
        (
            tref: ReferencedType,
            members: ReferencedTypeMembers,
            builder: CliMetadataBuilder,
            referencedTypeLookup: Dictionary<_, _>,
            assemblyRefLookup: Dictionary<AssemblyReference, _>
        )
        =
        match referencedTypeLookup.TryGetValue tref with
        | true, { TypeRef = i } -> i
        | false, _ ->
            let rscope =
                match tref.ResolutionScope with
                | TypeReferenceParent.Null -> ResolutionScope.Null
                | TypeReferenceParent.Assembly assem -> ResolutionScope.AssemblyRef assemblyRefLookup.[assem]
                | TypeReferenceParent.Type parent ->
                    this.SerializeReferencedType (
                        parent,
                        referencedTypes.[parent],
                        builder,
                        referencedTypeLookup,
                        assemblyRefLookup
                     )
                    |> ResolutionScope.TypeRef

            let i =
                builder.Tables.TypeRef.Add
                    { ResolutionScope = rscope
                      TypeName = builder.Strings.Add tref.TypeName
                      TypeNamespace = builder.Strings.Add tref.TypeNamespace }

            let members' = Dictionary<obj, TableIndex<MemberRefRow>>((*members.FieldCount +*) members.MethodCount)

            for method in members.Method do
                members'.[method] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add method.Name
                          Signature = invalidOp "TODO: How to get signature" }

            referencedTypeLookup.[tref] <- { TypeRef = i; Members = members' }
            i

    member private this.SerializeDefinedType
        (
            tdef: DefinedType,
            members: DefinedTypeMembers,
            indices: MemberIndices,
            builder: CliMetadataBuilder,
            namedTypeMapping,
            modifierTypeMapping,
            definedTypeLookup: Dictionary<_, _>,
            referencedTypeLookup,
            assemblyRefLookup,
            methodDefSignatures: Dictionary<_, _>
        ) =
        match definedTypeLookup.TryGetValue tdef with
        | true, { TypeDef = i } -> i
        | false, _ ->
            let extends =
                match tdef.Extends with
                | ClassExtends.Null -> Unchecked.defaultof<TypeDefOrRef>
                | ClassExtends.ConcreteDef(AsDefinedType tdef)
                | ClassExtends.AbstractDef(AsDefinedType tdef) ->
                    this.SerializeDefinedType (
                        tdef,
                        members,
                        indices,
                        builder,
                        namedTypeMapping,
                        modifierTypeMapping,
                        definedTypeLookup,
                        referencedTypeLookup,
                        assemblyRefLookup,
                        methodDefSignatures
                    )
                    |> TypeDefOrRef.Def
                | ClassExtends.ConcreteRef(AsReferencedType tref)
                | ClassExtends.AbstractRef(AsReferencedType tref) ->
                    this.SerializeReferencedType (
                        tref,
                        referencedTypes.[tref],
                        builder,
                        referencedTypeLookup,
                        assemblyRefLookup
                    )
                    |> TypeDefOrRef.Ref
                | ClassExtends.Spec tspec -> failwith "TODO: Implement writing of TypeSpecs"

            //members'
            let methods = Dictionary<DefinedMethod, TableIndex<MethodDefRow>> members.MethodCount

            for method in members.Method do
                let signature = method.Signature
                let i =
                    { Rva = MethodBodyLocation(noImpl "// TODO: Get method body somehow")
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature =
                        match methodDefSignatures.TryGetValue signature with
                        | true, offset -> offset
                        | false, _ ->
                            let signature' = Blob.mapMethodDefSig namedTypeMapping modifierTypeMapping &signature
                            let offset = builder.Blob.Add &signature'
                            methodDefSignatures.[signature] <- offset
                            offset
                      ParamList = noImpl "TODO: Get parameters, maybe have another dictionary" }
                    |> builder.Tables.MethodDef.Add
                if methods.Count = 0 then indices.MethodList <- i
                methods.[method] <- i
            
            let i =
                builder.Tables.TypeDef.Add
                    { Flags = tdef.Flags
                      TypeName = builder.Strings.Add tdef.TypeName
                      TypeNamespace = builder.Strings.Add tdef.TypeNamespace
                      Extends = extends
                      FieldList = indices.FieldList
                      MethodList = indices.MethodList }
            
            definedTypeLookup.[tdef] <-
                { TypeDef = i
                  //Fields = fields
                  Methods = methods }
            i

    member this.Serialize() = // TODO: Maybe make a ModuleBuilderSerializer class to store all of these, and to allow MemberIndices to be a struct again?
        let builder = CliMetadataBuilder(fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add this.Mvid))
        let assemblyRefLookup = Dictionary<AssemblyReference, TableIndex<AssemblyRefRow>> assemblyRefs.Count
        let referencedTypeLookup = Dictionary<ReferencedType, _> referencedTypes.Count
        let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count
        let methodDefSignatures = Dictionary<MethodDefSig<_, _>, MethodDefSigOffset> definedTypes.Count
        let mutable indices = MemberIndices()

        let inline serializeReferencedType tref members =
            this.SerializeReferencedType (
                tref,
                members,
                builder,
                referencedTypeLookup,
                assemblyRefLookup
            )

        let rec serializeDefinedType tdef members =
            this.SerializeDefinedType (
                tdef,
                definedTypes.[tdef],
                indices,
                builder,
                namedTypeMapping,
                modifierTypeMapping,
                definedTypeLookup,
                referencedTypeLookup,
                assemblyRefLookup,
                methodDefSignatures
            )

        and namedTypeMapping =
            function
            | DefinedType tdef ->
                MetadataSignatures.TypeDefOrRefEncoded.Def(serializeDefinedType tdef definedTypes.[tdef])
            | ReferencedType tref ->
                MetadataSignatures.TypeDefOrRefEncoded.Ref(serializeReferencedType tref referencedTypes.[tref])

        and modifierTypeMapping =
            function
            | TypeDefOrRefOrSpec.Def tdef -> TypeDefOrRef.Def(serializeDefinedType tdef definedTypes.[tdef])
            | TypeDefOrRefOrSpec.Ref tref -> TypeDefOrRef.Ref(serializeReferencedType tref referencedTypes.[tref])
            | TypeDefOrRefOrSpec.Spec tspec -> invalidOp "TODO: Get type specs"

        let assemblyDef =
            match assembly with
            | Some assem ->
                { HashAlgId = AssemblyHashAlgorithm.None // TODO: Set the HashAlgId properly
                  MajorVersion = assem.Version.Major
                  MinorVersion = assem.Version.Minor
                  BuildNumber = assem.Version.Build
                  RevisionNumber = assem.Version.Revision
                  Flags = if assem.PublicKey.IsDefaultOrEmpty then AssemblyFlags.None else AssemblyFlags.PublicKey // TODO: Allow setting of other assembly flags
                  PublicKey = builder.Blob.Add assem.PublicKey
                  Name = builder.Strings.Add assem.Name
                  Culture = builder.Strings.Add assem.Culture }
                |> builder.Tables.Assembly.Add
                |> ValueSome
            | None -> ValueNone

        for assem in assemblyRefs do
            assemblyRefLookup.[assem] <-
                builder.Tables.AssemblyRef.Add
                    { MajorVersion = assem.Version.Major
                      MinorVersion = assem.Version.Minor
                      BuildNumber = assem.Version.Build
                      RevisionNumber = assem.Version.Revision
                      PublicKeyOrToken = builder.Blob.Add assem.PublicKeyOrToken
                      Name = builder.Strings.Add assem.Name
                      Culture = builder.Strings.Add assem.Culture
                      HashValue = builder.Blob.Add assem.HashValue }

        for KeyValue(tref, members) in referencedTypes do
            this.SerializeReferencedType(tref, members, builder, referencedTypeLookup, assemblyRefLookup) |> ignore

        // TODO: Add <Module> type.

        for KeyValue(tdef, members) in definedTypes do serializeDefinedType tdef members |> ignore

        builder
