namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata

/// (II.25.3.3)
type CliHeaderFields =
    { // HeaderSize = 0x48u
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      // Metadata
      // Flags
      // EntryPointToken
      Resources: unit
      StrongNameSignature: ImmutableArray<byte>
      CodeManagerTable: uint64 // TODO: Figure out if this field should exist.
      VTableFixups: unit
      // ExportAddressTableJumps
      // ManagedNativeHeader
      }

    static member Default =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Resources = ()
          StrongNameSignature = ImmutableArray.Empty
          CodeManagerTable = 0UL
          VTableFixups = () }

/// (II.25.3.3.1)
[<Flags>]
type CorFlags =
    | None = 0u
    | ILOnly = 1u
    | Requires32Bit = 2u
    | StrongNameSigned = 0x8u
    | NativeEntryPoint = 0x10u
    | TrackDebugData = 0x10000u

// II.22.32
[<Struct; IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type NestedClass =
    { NestedClass: SimpleIndex<TypeDefRow>
      EnclosingClass: SimpleIndex<TypeDefRow> }

    interface IIndexValue with
        member this.CheckOwner owner =
            IndexOwner.checkIndex owner this.NestedClass
            IndexOwner.checkIndex owner this.EnclosingClass

/// <summary>(0x00) Represents the single row of the <c>Module</c> table (II.22.30).</summary>
type ModuleTable =
    { // Generation
      Name: Identifier
      Mvid: Guid
      // EncId
      // EncBaseId
      }

[<Sealed>]
type MetadataBuilderState (mdle: ModuleTable) =
    let owner = IndexOwner()
    let warnings = ImmutableArray.CreateBuilder<ValidationWarning>()
    let clsViolations = ImmutableArray.CreateBuilder<ClsViolation>()
    let mutable entrypoint = ValueNone

    let typeDef = TypeDefTable owner
    let mutable assembly = None

    member internal _.Owner = owner

    member val Header = CliHeaderFields.Default with get, set

    member this.HeaderFlags =
        let signed =
            if this.Header.StrongNameSignature.IsEmpty
            then CorFlags.None
            else CorFlags.StrongNameSigned
        CorFlags.ILOnly ||| signed

    /// The metadata version, contained in the metadata root (II.24.2.1).
    member val MetadataVersion = MetadataVersion.ofStr "v4.0.30319" with get, set

    member val Warnings = warnings
    member val ClsViolations = clsViolations

    // Reserved: uint32
    member val MajorVersion: byte = 2uy
    member val MinorVersion: byte = 0uy
    // HeapSizes: byte
    // Reserved: byte
    // Valid: uint64
    // Sorted: uint64 // TODO: Figure out what Sorted is used for.
    // Rows
    /// (0x00)
    member val Module = mdle
    /// (0x01)
    member val TypeRef: TypeRefTable = TypeRefTable(owner, warnings)
    /// (0x02)
    member _.TypeDef: TypeDefTable = typeDef
    // (0x04)
    member val Field = OwnedTableBuilder<TypeDefRow, FieldRow> owner
    // (0x06)
    member val Method = OwnedTableBuilder<TypeDefRow, MethodDefRow> owner
    // (0x08)
    // member Param
    // (0x09)
    // member InterfaceImpl
    /// (0x0A)
    member val MemberRef: MemberRefTable = MemberRefTable(owner, warnings)
    // (0x0B)
    // member Constant
    /// (0x0C)
    member val CustomAttribute: CustomAttributeTable = CustomAttributeTable owner
    // (0x0D)
    // member FieldMarshal
    // (0x0E)
    // member DeclSecurity
    // (0x0F)
    // member ClassLayout
    // (0x10)
    // member FieldLayout
    // (0x11)
    // member StandAloneSig
    // (0x12)
    // member EventMap
    // (0x14)
    // member Event
    // (0x15)
    // member PropertyMap
    // (0x17)
    // member Property
    // (0x18)
    // member MethodSemantics
    // (0x19)
    // member MethodImpl
    /// (0x1A)
    member val ModuleRef = ModuleRefTable owner
    // (0x1B)
    // member TypeSpec
    // (0x1C)
    // member ImplMap
    // (0x1D)
    // member FieldRva
    /// (0x20)
    member _.Assembly: Assembly option = assembly
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// (0x23)
    member val AssemblyRef: AssemblyRefTable = AssemblyRefTable(owner, warnings)
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    /// (0x26)
    member val File = FileTable owner
    // (0x27)
    // member ExportedType
    // (0x28)
    // member ManifestResource
    /// (0x29)
    member val NestedClass =
        Seq.choose
            (fun (tdef: TypeDefRow) ->
                match tdef.EnclosingClass with
                | Some parent ->
                    { NestedClass = SimpleIndex(owner, tdef)
                      EnclosingClass = parent }
                    |> Some
                | _ -> None)
            typeDef
    // (0x2A)
    // member GenericParam // TODO: Create custom table type for generic parameters.
    // (0x2B)
    // member MethodSpec
    // (0x2C)
    // member GenericParamConstraint

    /// <summary>Gets or sets the entrypoint of the assembly.</summary>
    /// <remarks>The entrypoint of the assembly is specified by the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</remarks>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException" />
    member _.EntryPoint
        with get(): EntryPointToken voption = entrypoint
        and set (main: EntryPointToken voption) =
            ValueOption.iter (IndexOwner.checkOwner owner) main
            entrypoint <- main

    member _.SetAssembly(assm: Assembly) = assembly <- Some assm; AssemblyIndex(owner, ())

    member internal this.FindType t: SimpleIndex<_> option =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateTable table = ImmutableTable(table, fun item -> SimpleIndex(owner, item))
