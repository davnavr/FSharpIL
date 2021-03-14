namespace FSharpIL.Metadata

open System.Collections.Immutable

// TODO: Make a computation expression for Result<_, _> or ValidationResult<_>
// TODO: If making a module for unsafe functions, consider using a CompilerMessageAttribute as a warning.
/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata (builder: CliMetadataBuilder) =
    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let methodDef = builder.Method.ToImmutable()
    let parameters =
        methodDef.Rows
        |> Seq.collect(fun method -> Seq.indexed method.ParamList)
        |> ImmutableArray.CreateRange

    let nestedClass = builder.NestedClass |> ImmutableArray.CreateRange

    let valid, rowCounts =
        // Module table is always present
        let mutable bits = 1UL
        let counts = ImmutableArray.CreateBuilder 48
        counts.Add 1u

        if builder.TypeRef.Count > 0 then
            bits <- bits ||| (1UL <<< 1)
            uint32 builder.TypeRef.Count |> counts.Add
        if builder.TypeDef.Count > 0 then
            bits <- bits ||| (1UL <<< 2)
            uint32 builder.TypeDef.Count |> counts.Add
        if builder.Field.Count > 0 then
            bits <- bits ||| (1UL <<< 4)
            uint32 builder.Field.Count |> counts.Add
        if methodDef.Count > 0 then
            bits <- bits ||| (1UL <<< 6)
            uint32 methodDef.Count |> counts.Add
        if not parameters.IsEmpty then
            bits <- bits ||| (1UL <<< 8)
            uint32 parameters.Length |> counts.Add



        if builder.MemberRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0xA)
            uint32 builder.MemberRef.Count |> counts.Add



        if builder.CustomAttribute.Count > 0 then
            bits <- bits ||| (1UL <<< 0xC)
            uint32 builder.CustomAttribute.Count |> counts.Add



        if builder.ModuleRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0x1A)
            uint32 builder.ModuleRef.Count |> counts.Add
        if builder.TypeSpec.Count > 0 then
            bits <- bits ||| (1UL <<< 0x1B)
            uint32 builder.TypeSpec.Count |> counts.Add


        if builder.Assembly.IsSome then
            bits <- bits ||| (1UL <<< 0x20)
            counts.Add 1u
        if builder.AssemblyRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0x23)
            uint32 builder.AssemblyRef.Count |> counts.Add



        if builder.File.Count > 0 then
            bits <- bits ||| (1UL <<< 0x26)
            uint32 builder.File.Count |> counts.Add



        if builder.MethodSpec.Count > 0 then
            bits <- bits ||| (1UL <<< 0x2B)
            uint32 builder.MethodSpec.Count |> counts.Add



        if nestedClass.Length > 0 then
            bits <- bits ||| (1UL <<< 0x29)
            uint32 nestedClass.Length |> counts.Add

        bits, counts

    member val internal Owner = builder.Owner
    member val Header = builder.Header
    /// <summary>Corresponds to the <c>Flags</c> field of the CLI header (II.25.3.3).</summary>
    member val HeaderFlags = builder.HeaderFlags
    /// <summary>Corresponds to the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</summary>
    member val EntryPointToken = builder.EntryPointToken

    /// <summary>Corresponds to the <c>Version</c> field of the metadata root (II.24.2.1)</summary>
    member val MetadataVersion = builder.MetadataVersion

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

    member val MemberRef = builder.MemberRef.ToImmutable()

    member val CustomAttribute = builder.CustomAttribute.ToImmutableArray()

    member val ModuleRef = builder.ModuleRef.ToImmutable()
    member val TypeSpec = builder.TypeSpec.ToImmutable()

    member val Assembly = builder.Assembly
    member val AssemblyRef = builder.AssemblyRef.ToImmutable()

    member val File = builder.File.ToImmutable()

    member val MethodSpec = builder.MethodSpec.ToImmutable()

    member _.NestedClass = nestedClass

    /// Gets a bit vector that indicates which tables are present (II.24.2.6).
    member _.Valid: uint64 = valid

    /// <summary>
    /// Corresponds to the <c>Rows</c> field of the <c>#~</c> stream header,
    /// which specifies "the number of rows for each present table" (II.24.2.6).
    /// </summary>
    member _.RowCounts = rowCounts
