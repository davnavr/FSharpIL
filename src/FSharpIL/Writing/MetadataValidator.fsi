namespace FSharpIL.Writing

open FSharpIL.Cli
open FSharpIL.Metadata

type IMetadataValidator<'T> = interface
    abstract Validate: 'T * ValidationWarningsBuilder option -> IValidationError option
    abstract Duplicate: 'T -> IValidationError
end

type [<Struct>] internal DefinedTypeValidator = interface IMetadataValidator<DefinedType>
type [<Struct>] internal ReferencedTypeValidator = interface IMetadataValidator<ReferencedType>
