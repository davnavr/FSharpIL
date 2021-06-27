namespace rec FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
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
type TypeEntry<'Row, 'Members when 'Row :> ITableRow> = { Row: TableIndex<'Row>; Members: 'Members }

[<IsByRefLike>]
type MemberIndices = struct
    [<DefaultValue>] val mutable FieldList: TableIndex<FieldRow>
    [<DefaultValue>] val mutable MethodList: TableIndex<MethodDefRow>
    [<DefaultValue>] val mutable EventList: TableIndex<EventRow>
    [<DefaultValue>] val mutable PropertyList: TableIndex<PropertyRow>
end

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
            () // TODO: Warning for duplicate assembly reference

    member private this.SerializeReferencedType
        (
            tref: ReferencedType,
            members: ReferencedTypeMembers,
            builder: CliMetadataBuilder,
            referencedTypeLookup: Dictionary<_, TypeEntry<TypeRefRow, _>>,
            assemblyRefLookup: Dictionary<AssemblyReference, _>
        )
        =
        match referencedTypeLookup.TryGetValue tref with
        | true, { Row = i } -> i
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

            referencedTypeLookup.[tref] <- { Row = i; Members = members' }
            i

    member private this.SerializeDefinedType
        (
            tdef: DefinedType,
            members: DefinedTypeMembers,
            indices: byref<MemberIndices>,
            builder: CliMetadataBuilder,
            definedTypeLookup: Dictionary<_, TypeEntry<_, _>>,
            referencedTypeLookup,
            assemblyRefLookup
        ) =
        match definedTypeLookup.TryGetValue tdef with
        | true, { Row = i } -> i
        | false, _ ->
            let extends =
                match tdef.Extends with
                | ClassExtends.Null -> Unchecked.defaultof<TypeDefOrRef>
                | ClassExtends.ConcreteDef(DefinedType tdef)
                | ClassExtends.AbstractDef(DefinedType tdef) ->
                    this.SerializeDefinedType (
                        tdef,
                        members,
                        &indices,
                        builder,
                        definedTypeLookup,
                        referencedTypeLookup,
                        assemblyRefLookup
                    )
                    |> TypeDefOrRef.Def
                | ClassExtends.ConcreteRef(ReferencedType tref)
                | ClassExtends.AbstractRef(ReferencedType tref) ->
                    this.SerializeReferencedType(tref, referencedTypes.[tref], builder, referencedTypeLookup, assemblyRefLookup)
                    |> TypeDefOrRef.Ref
                | ClassExtends.Spec tspec -> failwith "TODO: Implement writing of TypeSpecs"

            //fields
            let methods = Dictionary<DefinedMethod, TableIndex<MethodDefRow>> members.MethodCount

            for method in members.Method do
                let i =
                    { Rva = MethodBodyLocation(noImpl "// TODO: Get method body somehow")
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature = noImpl "TODO: Get method signature"
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
            
            // TODO: Allow modification of existing rows, to defer setting of Extends, FieldList, and MethodList (though an iteration through the list of types might be needed again).
            definedTypeLookup.[tdef] <-
                { Row = i; Members = () } // TODO: Add typedef members
            i

    member this.Serialize() =
        let builder = CliMetadataBuilder(fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add this.Mvid))
        let assemblyRefLookup = Dictionary<AssemblyReference, TableIndex<AssemblyRefRow>> assemblyRefs.Count
        let referencedTypeLookup = Dictionary<ReferencedType, TypeEntry<TypeRefRow, _>> referencedTypes.Count
        let definedTypeLookup = Dictionary<DefinedType, TypeEntry<TypeDefRow, _>> definedTypes.Count

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

        let mutable indices = MemberIndices()
        for KeyValue(tdef, members) in definedTypes do
            this.SerializeDefinedType (
                tdef,
                members,
                &indices,
                builder,
                definedTypeLookup,
                referencedTypeLookup,
                assemblyRefLookup
            )
            |> ignore

        builder
