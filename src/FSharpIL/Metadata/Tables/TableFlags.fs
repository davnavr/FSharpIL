namespace FSharpIL.Metadata.Tables

open System

[<Flags>]
type ValidTableFlags =
    | None = 0UL
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

    | ClassLayout = 0x8000UL

    | StandAloneSig = 0x2_0000UL
    | EventMap = 0x4_0000UL
    | Event = 0x10_0000UL
    | PropertyMap = 0x20_0000UL
    | Property = 0x80_0000UL
    | MethodSemantics = 0x100_0000UL
    | MethodImpl = 0x200_0000UL
    | ModuleRef = 0x400_0000UL
    | TypeSpec = 0x800_0000UL

    | FieldRva = 0x2000_0000UL
    | Assembly = 0x1_0000_0000UL
    | AssemblyRef = 0x8_0000_0000UL

    | File = 0x40_0000_0000UL
    | ExportedType = 0x80_0000_0000UL

    | ManifestResource = 0x100_0000_0000UL
    | NestedClass = 0x200_0000_0000UL
    | GenericParam = 0x400_0000_0000UL
    | MethodSpec = 0x800_0000_0000UL
    | GenericParamConstraint = 0x1000_0000_0000UL

    | Document = 0x1_0000_0000_0000UL
    | MethodDebugInformation = 0x2_0000_0000_0000UL
