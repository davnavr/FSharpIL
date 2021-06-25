namespace FSharpIL.Writing

open FSharpIL.Cli
open FSharpIL.Metadata

type IMetadataValidator<'T when 'T : not struct> = interface
    abstract Validate: 'T * ValidationWarningsBuilder option -> IValidationError option
    abstract Duplicate: 'T -> IValidationError
end

[<System.ObsoleteAttribute>]
type [<Struct>] internal DefinedTypeValidator = interface IMetadataValidator<DefinedType>
[<System.ObsoleteAttribute>]
type [<Struct>] internal ReferencedTypeValidator = interface IMetadataValidator<ReferencedType>
