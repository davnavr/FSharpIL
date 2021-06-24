namespace FSharpIL.Writing

open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata

type IMetadataValidator<'T when 'T : not struct> = interface
    abstract Validate: 'T * warnings: ValidationWarningsBuilder option -> IValidationError option
    abstract Duplicate: 'T -> IValidationError
end

type DefinedTypeValidator = struct
    interface IMetadataValidator<DefinedType> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Validate(tdef, warnings) = raise (System.NotImplementedException())
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Duplicate(arg1) = raise (System.NotImplementedException())
end

type ReferencedTypeValidator = struct
    interface IMetadataValidator<ReferencedType> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Validate(tref, warnings) = raise (System.NotImplementedException())
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Duplicate(arg1) = raise (System.NotImplementedException())
end
