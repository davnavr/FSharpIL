namespace rec FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers =
    val private owner: DefinedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable methods: HybridHashSet<DefinedMethod>

    member this.MethodCount = this.methods.Count

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.AddMethod(method: DefinedMethod) = // TODO: return something like a struct(DefinedType * DefinedMethod)
        match this.owner with
        //| DefinedType.Enum _ -> noImpl "error for enum cannot have methods"
        //| DefinedType.Delegate _ -> noImpl "error for delegate cannot have additional methods maybe go check if ECMA-335 prohibits this in the list of rules, since section I says so"
        | :? TypeDefinition<TypeKinds.StaticClass> ->
            match method with
            | :? MethodDefinition<MethodKinds.Static> ->
                if this.methods.Add method
                then ValidationResult.Ok()
                else noImpl "error for duplicate method"
            | _ -> noImpl "bad"
        | _ -> noImpl "bad"

[<Sealed>]
type ReferencedTypeMembers internal (warnings) = class end

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeEntry<'Row, 'Members when 'Row :> ITableRow> = { Row: TableIndex<'Row>; Members: 'Members }

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
        let assemblyRefLookup = Dictionary<AssemblyReference, TableIndex<AssemblyRefRow>> assemblyRefs.Count
        let definedTypeLookup = Dictionary<DefinedType, TypeEntry<TypeDefRow, _>> definedTypes.Count

        let assemblyDef =
            match assembly with
            | Some assem ->
                { HashAlgId = AssemblyHashAlgorithm.SHA1 // TODO: Set the HashAlgId properly
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

        let mutable fieldList, methodList, propertyList, eventList =
            Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>, Unchecked.defaultof<_>

        for KeyValue(tdef, members) in definedTypes do
            let methods = Dictionary<DefinedMethod, TableIndex<MethodDefRow>> members.MethodCount

            for method in members.methods do
                let i =
                    { Rva = MethodBodyLocation(noImpl "// TODO: Get method body somehow")
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature = noImpl "TODO: Get method signature"
                      ParamList = noImpl "TODO: Get parameters, maybe have another dictionary" }
                    |> builder.Tables.MethodDef.Add
                if methods.Count = 0 then methodList <- i
                methods.[method] <- i

            definedTypeLookup.[tdef] <-
                { Row =
                    { Flags = tdef.Flags
                      TypeName = builder.Strings.Add tdef.TypeName
                      TypeNamespace = builder.Strings.Add tdef.TypeNamespace
                      Extends = failwith "TODO: How to get extends value for type that will be added later?"
                      FieldList = fieldList
                      MethodList = methodList }
                    |> builder.Tables.TypeDef.Add
                  Members = failwith "bad" }

        builder
