namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Cil

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<AbstractClass>]
type DefinedMethodBody =
    val InitLocals: InitLocals
    val LocalTypes: Signatures.LocalVarSig
    new (initLocals, localTypes) = { InitLocals = initLocals; LocalTypes = localTypes }
    new () = { InitLocals = SkipInitLocals; LocalTypes = ImmutableArray.Empty }
    abstract WriteInstructions: byref<MethodBodyBuilder> -> uint16

[<Sealed>]
type DefinedTypeMembers =
    val private owner: DefinedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.MethodCount = this.Method.Count

    member this.AddMethod(method: DefinedMethod, body: DefinedMethodBody voption) = // TODO: return something like a struct(DefinedType * DefinedMethod)
        match this.owner with
        //| DefinedType.Enum _ -> noImpl "error for enum cannot have methods"
        //| DefinedType.Delegate _ -> noImpl "error for delegate cannot have additional methods maybe go check if ECMA-335 prohibits this in the list of rules, since section I says so"
        | :? TypeDefinition<TypeKinds.StaticClass> ->
            match method with
            | :? MethodDefinition<MethodKinds.Static> ->
                match this.Method.Add method, body with
                | true, ValueSome body' ->
                    this.MethodBodyLookup.Inner.[method] <- body'
                    ValidationResult.Ok()
                | true, ValueNone -> noImpl "error for missing method body"
                | false, _ -> noImpl "error for duplicate method"
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

type MemberIndices = struct
    [<DefaultValue>] val mutable FieldList: TableIndex<FieldRow>
    [<DefaultValue>] val mutable MethodList: TableIndex<MethodDefRow>
    [<DefaultValue>] val mutable EventList: TableIndex<EventRow>
    [<DefaultValue>] val mutable PropertyList: TableIndex<PropertyRow>
end

type ModuleBuilderSerializer
    (
        name: Identifier,
        mvid,
        assembly: AssemblyDefinition option,
        assemblyReferences: HashSet<AssemblyReference>,
        definedTypes: Dictionary<DefinedType, DefinedTypeMembers>,
        referencedTypes: Dictionary<ReferencedType, ReferencedTypeMembers>
    )
    as serializer
    =
    let builder = CliMetadataBuilder(fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add mvid))
    let assemblyDefIndex =
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

    let assemblyReferenceLookup = Dictionary<AssemblyReference, TableIndex<AssemblyRefRow>> assemblyReferences.Count
    let referencedTypeLookup = Dictionary<ReferencedType, _> referencedTypes.Count
    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count

    let methodDefSignatures = Dictionary<Signatures.MethodDefSig, MethodDefSigOffset>(definedTypeLookup.Count * 8)
    let localVarSignatures = Dictionary<Signatures.LocalVarSig, TableIndex<StandaloneSigRow>>(methodDefSignatures.Count)

    let mutable indices = MemberIndices()

    let namedTypeMapping =
        function
        | DefinedType tdef ->
            MetadataSignatures.TypeDefOrRefEncoded.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | ReferencedType tref ->
            MetadataSignatures.TypeDefOrRefEncoded.Ref(serializer.SerializeReferencedType(tref, referencedTypes.[tref]))

    let modifierTypeMapping =
        function
        | TypeDefOrRefOrSpec.Def tdef -> TypeDefOrRef.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | TypeDefOrRefOrSpec.Ref tref -> TypeDefOrRef.Ref(serializer.SerializeReferencedType(tref, referencedTypes.[tref]))
        | TypeDefOrRefOrSpec.Spec tspec -> invalidOp "TODO: Get type specs"

    member private _.SerializeAssemblyReferences() =
        for assem in assemblyReferences do
            assemblyReferenceLookup.[assem] <-
                builder.Tables.AssemblyRef.Add
                    { MajorVersion = assem.Version.Major
                      MinorVersion = assem.Version.Minor
                      BuildNumber = assem.Version.Build
                      RevisionNumber = assem.Version.Revision
                      PublicKeyOrToken = builder.Blob.Add assem.PublicKeyOrToken
                      Name = builder.Strings.Add assem.Name
                      Culture = builder.Strings.Add assem.Culture
                      HashValue = builder.Blob.Add assem.HashValue }

    member private this.SerializeReferencedType(tref, members) =
        match referencedTypeLookup.TryGetValue tref with
        | true, { TypeRef = i } -> i
        | false, _ ->
            let rscope =
                match tref.ResolutionScope with
                | TypeReferenceParent.Null -> ResolutionScope.Null
                | TypeReferenceParent.Assembly assem -> ResolutionScope.AssemblyRef assemblyReferenceLookup.[assem]
                | TypeReferenceParent.Type parent ->
                    ResolutionScope.TypeRef(this.SerializeReferencedType(parent, referencedTypes.[parent]))

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

    member private this.SerializeDefinedType(tdef: DefinedType, members: DefinedTypeMembers) =
        match definedTypeLookup.TryGetValue tdef with
        | true, { TypeDef = i } -> i
        | false, _ ->
            let extends =
                match tdef.Extends with
                | ClassExtends.Null -> Unchecked.defaultof<TypeDefOrRef>
                | ClassExtends.ConcreteDef(AsDefinedType tdef)
                | ClassExtends.AbstractDef(AsDefinedType tdef) -> TypeDefOrRef.Def(this.SerializeDefinedType(tdef, members))
                | ClassExtends.ConcreteRef(AsReferencedType tref)
                | ClassExtends.AbstractRef(AsReferencedType tref) ->
                    TypeDefOrRef.Ref(this.SerializeReferencedType(tref, referencedTypes.[tref]))
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

    member this.Serialize() =
        this.SerializeAssemblyReferences()

        for KeyValue(tref, members) in referencedTypes do this.SerializeReferencedType(tref, members) |> ignore

        // TODO: Add <Module> type.

        for KeyValue(tdef, members) in definedTypes do this.SerializeDefinedType(tdef, members) |> ignore

        builder

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
    let referencedTypes = Dictionary<ReferencedType, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)

    static member val internal ModuleTypeName = Identifier.ofStr "<Module>"

    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val Mvid = Option.defaultWith Guid.NewGuid mvid
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

    member internal this.Serialize() =
        ModuleBuilderSerializer(name, this.Mvid, assembly, assemblyRefs, definedTypes, referencedTypes).Serialize()
