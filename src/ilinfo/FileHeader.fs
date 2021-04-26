namespace ILInfo

open Argu

[<RequireQualifiedAccess>]
type FileHeader =
    | [<Unique>] Coff
    | [<Unique>] Standard
    | [<Unique>] NT_Specific
    | [<Unique>] Data_Directories
    | [<Unique>] Section_Headers

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Coff -> "include the values of the COFF header fields in the output."
            | Standard -> "includes the values of the optional header standard fields in the output."
            | NT_Specific -> "includes the values of the optional header NT-specific fields in the output."
            | Section_Headers -> "includes the values of the section headers' fields in the output."
            | Data_Directories -> "includes the RVAs and sizes of each data directory in the output."
