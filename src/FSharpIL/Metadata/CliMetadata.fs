namespace FSharpIL.Metadata

open System.Collections.Immutable

[<System.Flags>]
type MetadataTableFlags =
    | Module = 1UL
    | TypeRef = 2UL
    | TypeDef = 4UL
    | Field = 0x10UL // (1UL <<< 4)
    | MethodDef = 0x40UL
    | Param = 0x100UL
    | InterfaceImpl = 0x200UL
    | MemberRef = 0x400UL
    | Constant = 0x800UL
    | CustomAttribute = 0x1000UL

    | StandAloneSig = 0x20000UL
    | EventMap = 0x40000UL
    | Event = 0x100000UL
    | PropertyMap = 0x200000UL
    | Property = 0x800000UL
    | MethodSemantics = 0x1000000UL

    | ModuleRef = 0x4000000UL
    | TypeSpec = 0x8000000UL

    | Assembly = 0x100000000UL
    | AssemblyRef = 0x800000000UL

    | File = 0x4000000000UL

    | NestedClass = 0x20000000000UL
    | GenericParam = 0x40000000000UL
    | MethodSpec = 0x80000000000UL
    | GenericParamConstraint = 0x100000000000UL

// TODO: Make a computation expression for Result<_, _> or ValidationResult<_>
// TODO: If making a module for unsafe functions, consider using a CompilerMessageAttribute as a warning.
/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata (builder: CliMetadataBuilder) =
    let blobs = builder.Blobs.ToImmutable()

    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let methodDef = builder.Method.ToImmutable()
    let parameters =
        methodDef.Rows
        |> Seq.collect (fun method -> method.GetParameters blobs.MethodDefSig.[method.Signature])
        |> ImmutableArray.CreateRange

    let standAloneSig = builder.StandAloneSig.ToImmutable blobs.LocalVarSig
    let eventMap = builder.EventMap.ToImmutable()
    let propertyMap = builder.PropertyMap.ToImmutable()

    let nestedClass = ImmutableArray.CreateRange builder.NestedClass
    let genericParam = builder.GenericParam.ToImmutable()
    let genericParamConstraints = builder.GenericParam.GetConstraints()

    let valid, rowCounts =
        // Module table is always present
        let mutable bits = MetadataTableFlags.Module
        let counts = ImmutableArray.CreateBuilder 48
        counts.Add 1u

        if builder.TypeRef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.TypeRef
            uint32 builder.TypeRef.Count |> counts.Add
        if builder.TypeDef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.TypeDef
            uint32 builder.TypeDef.Count |> counts.Add
        if builder.Field.Count > 0 then
            bits <- bits ||| MetadataTableFlags.Field
            uint32 builder.Field.Count |> counts.Add
        if methodDef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.MethodDef
            uint32 methodDef.Count |> counts.Add
        if not parameters.IsEmpty then
            bits <- bits ||| MetadataTableFlags.Param
            uint32 parameters.Length |> counts.Add
        if builder.InterfaceImpl.Count > 0 then
            bits <- bits ||| MetadataTableFlags.InterfaceImpl
            uint32 builder.InterfaceImpl.Count |> counts.Add
        if builder.MemberRef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.MemberRef
            uint32 builder.MemberRef.Count |> counts.Add
        if builder.Constant.Count > 0 then
            bits <- bits ||| MetadataTableFlags.Constant
            uint32 builder.Constant.Count |> counts.Add
        if builder.CustomAttribute.Count > 0 then
            bits <- bits ||| MetadataTableFlags.CustomAttribute
            uint32 builder.CustomAttribute.Count |> counts.Add



        if standAloneSig.TotalCount > 0 then
            bits <- bits ||| MetadataTableFlags.StandAloneSig
            uint32 standAloneSig.TotalCount |> counts.Add

        if builder.EventMap.Owners.Count > 0 then
            bits <- bits ||| MetadataTableFlags.EventMap
            uint32 builder.EventMap.Owners.Count |> counts.Add

        if builder.EventMap.Count > 0 then
            bits <- bits ||| MetadataTableFlags.Event
            uint32 builder.EventMap.Count |> counts.Add

        if builder.PropertyMap.Owners.Count > 0 then
            bits <- bits ||| MetadataTableFlags.PropertyMap
            uint32 builder.PropertyMap.Owners.Count |> counts.Add

        if builder.PropertyMap.Count > 0 then
            bits <- bits ||| MetadataTableFlags.Property
            uint32 builder.PropertyMap.Count |> counts.Add

        if builder.MethodSemantics.Count > 0 then
            bits <- bits ||| MetadataTableFlags.MethodSemantics
            uint32 builder.MethodSemantics.Count |> counts.Add



        if builder.ModuleRef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.ModuleRef
            uint32 builder.ModuleRef.Count |> counts.Add
        if builder.TypeSpec.Count > 0 then
            bits <- bits ||| MetadataTableFlags.TypeSpec
            uint32 builder.TypeSpec.Count |> counts.Add


        if builder.Assembly.IsSome then
            bits <- bits ||| MetadataTableFlags.Assembly
            counts.Add 1u
        if builder.AssemblyRef.Count > 0 then
            bits <- bits ||| MetadataTableFlags.AssemblyRef
            uint32 builder.AssemblyRef.Count |> counts.Add



        if builder.File.Count > 0 then
            bits <- bits ||| MetadataTableFlags.File
            uint32 builder.File.Count |> counts.Add



        if nestedClass.Length > 0 then
            bits <- bits ||| MetadataTableFlags.NestedClass
            uint32 nestedClass.Length |> counts.Add

        if genericParam.Count > 0 then
            bits <- bits ||| MetadataTableFlags.GenericParam
            uint32 genericParam.Count |> counts.Add

        if builder.MethodSpec.Count > 0 then
            bits <- bits ||| MetadataTableFlags.MethodSpec
            uint32 builder.MethodSpec.Count |> counts.Add

        if genericParamConstraints.Count > 0 then
            bits <- bits ||| MetadataTableFlags.GenericParamConstraint
            uint32 genericParamConstraints.Count |> counts.Add

        bits, counts.ToImmutable()

    member val Header = builder.Header
    /// <summary>Corresponds to the <c>Flags</c> field of the CLI header (II.25.3.3).</summary>
    member val HeaderFlags = builder.HeaderFlags
    /// <summary>Corresponds to the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</summary>
    member val EntryPointToken = builder.EntryPointToken

    /// <summary>Corresponds to the <c>Version</c> field of the metadata root (II.24.2.1)</summary>
    member val MetadataVersion = builder.MetadataVersion

    member _.Blobs = blobs

    /// <summary>Corresponds to the <c>MajorVersion</c> field of the <c>#~</c> stream header.</summary>
    member val MajorVersion = builder.MajorVersion
    /// <summary>Corresponds to the <c>MinorVersion</c> field of the <c>#~</c> stream header.</summary>
    member val MinorVersion = builder.MinorVersion

    member val Module = builder.Module
    member val TypeRef = builder.TypeRef.ToImmutable()
    member val TypeDef = builder.TypeDef.ToImmutable()
    member val Field = builder.Field.ToImmutable()
    member _.MethodDef = methodDef
    member _.Param = parameters
    member val InterfaceImpl = builder.InterfaceImpl.ToImmutable()
    member val MemberRef = builder.MemberRef.ToImmutable()
    member val Constant = builder.Constant.ToImmutable()
    member val CustomAttribute = builder.CustomAttribute.ToImmutable()

    member _.StandAloneSig = standAloneSig
    member _.EventMap = eventMap
    member val Event = MetadataTable eventMap.Rows
    member _.PropertyMap = propertyMap
    member val Property = MetadataTable propertyMap.Rows
    member val MethodSemantics = builder.MethodSemantics.ToImmutable()

    member val ModuleRef = builder.ModuleRef.ToImmutable()
    member val TypeSpec = builder.TypeSpec.ToImmutable()

    member val Assembly = builder.Assembly
    member val AssemblyRef = builder.AssemblyRef.ToImmutable()

    member val File = builder.File.ToImmutable()

    member _.NestedClass = nestedClass
    member _.GenericParam = genericParam
    member val MethodSpec = builder.MethodSpec.ToImmutable()
    member _.GenericParamConstraint: MetadataTable<_> = genericParamConstraints

    /// Gets a bit vector that indicates which tables are present (II.24.2.6).
    member _.Valid: MetadataTableFlags = valid

    /// <summary>
    /// Corresponds to the <c>Rows</c> field of the <c>#~</c> stream header,
    /// which specifies "the number of rows for each present table" (II.24.2.6).
    /// </summary>
    member _.RowCounts = rowCounts
