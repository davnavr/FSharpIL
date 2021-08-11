module FSharpIL.Writing.BuildErrors

open FSharpIL.Metadata

open FSharpIL.Cli

/// Error used when an entry point method is added to a generic type.
[<RequireQualifiedAccess>]
type GenericTypeCannotHaveEntryPoint =
    { Owner: DefinedType
      Method: EntryPointMethod }

    override this.ToString() =
        sprintf
            "Cannot add entry point method %O to the generic type %O, only non-generic types can contain the entry point of the \
            assembly"
            this.Method
            this.Owner

    interface IValidationError
