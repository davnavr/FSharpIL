namespace FSharpIL.Writing.Tables

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when an invalid combination of <c>MethodAttributes</c> is used (6, 7, 24).</summary>
/// <category>Errors</category>
type InvalidMethodAttributesCombination = InvalidFlagsCombination<MethodDefFlags>

[<RequireQualifiedAccess>]
module private InvalidMethodFlags =
    let [<Literal>] StaticFinal = MethodDefFlags.Static ||| MethodDefFlags.Final
    let [<Literal>] StaticVirtual = MethodDefFlags.Static ||| MethodDefFlags.Virtual
    let [<Literal>] StaticNewSlot = MethodDefFlags.Static ||| MethodDefFlags.NewSlot
    let [<Literal>] FinalAbstract = MethodDefFlags.Final ||| MethodDefFlags.Abstract
    let [<Literal>] AbstractPInvokeImpl = MethodDefFlags.Abstract ||| MethodDefFlags.PInvokeImpl
    let [<Literal>] CompilerControlledSpecialName = MethodDefFlags.CompilerControlled ||| MethodDefFlags.SpecialName
    let [<Literal>] CompilerControlledRTSpecialName = MethodDefFlags.CompilerControlled ||| MethodDefFlags.RTSpecialName
    let [<Literal>] VirtualPInvokeImpl = MethodDefFlags.Virtual ||| MethodDefFlags.PInvokeImpl

/// <summary>Error used when an abstract method is not virtual (8, 23).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type MethodMustBeVirtual =
    { Flags: MethodDefFlags }
    override this.ToString() = sprintf "%A methods must also be virtual" this.Flags
    interface IValidationError

/// <summary>Error used when a method is marked <c>RTSpecialName</c> but is not marked <c>SpecialName</c> (9).</summary>
/// <category>Errors</category>
type RTSpecialNameMethodMustBeSpecialName () =
    override _.ToString() = "Methods marked RTSpecialName must also be marked SpecialName"
    interface IValidationError

/// <summary>Error used when a duplicate row is added to the <c>MethodDef</c> table (21).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateMethodDefRow =
    { Row: MethodDefRow }
    override this.ToString() =
        sprintf "A method with the same owner, name %O, and signature %O already exists" this.Row.Name this.Row.Signature
    interface IValidationError

/// <summary>
/// Error used when a non-abstract method is either missing a method body, not a PInvoke method, or is not implemented in the
/// runtime (25).
/// </summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type NonAbstractMethodIsMissingImplementation =
    { Row: MethodDefRow }
    override _.ToString() = "A non-abstract method must either have a non-zero RVA, be a PInvoke method, or be implemented in the runtime."
    interface IValidationError

/// <summary>
/// Error used when a method marked <c>CompilerControlled</c> is either missing a method body or is not a PInvoke method (26).
/// </summary>
/// <category>Errors</category>
type CompilerControlledMethodHasIncorrectImplementation =
    { Row: MethodDefRow }
    override this.ToString() =
        sprintf 
            "A compiler controlled method must either have a non-zero RVA or be a PInvoke method. The method row had a %s RVA and was %sa PInvoke method"
            (if this.Row.Rva.IsZero then "zero" else "non-zero")
            (if Flags.set MethodDefFlags.PInvokeImpl this.Row.Flags then System.String.Empty else "not ")
    interface IValidationError

/// <summary>
/// Error used when a method with a <c>Rva</c> of zero is not abstract, does not have an implementation of <c>Runtime</c>, or is
/// not a PInvoke method (33).
/// </summary>
/// <category>Errors</category>
type ZeroRvaMethodHasInvalidFlags =
    { Row: MethodDefRow }
    override _.ToString() =
        "Methods with a RVA of zero must either be abstract, have runtime implementation, or be a PInvoke method"
    interface IValidationError

// 34
// 35

[<RequireQualifiedAccess>]
module private MethodValidation =
    let nonAbstractMethod (row: inref<MethodDefRow>) =
        let mutable conditions = if row.Rva.IsZero then 0uy else 1uy // 25a
        if Flags.set MethodDefFlags.PInvokeImpl row.Flags then conditions <- conditions + 1uy // 25b
        if Flags.set MethodImplFlags.Runtime row.ImplFlags then conditions <- conditions + 1uy // 25c
        conditions = 1uy

    let compilerControlledMethod (row: inref<MethodDefRow>) =
        let mutable conditions = if row.Rva.IsZero then 0uy else 1uy
        if Flags.set MethodDefFlags.PInvokeImpl row.Flags then conditions <- conditions + 1uy
        conditions = 1uy

    let zeroRvaMethod (row: inref<MethodDefRow>) =
        let mutable conditions = if Flags.set MethodDefFlags.Abstract row.Flags then 1uy else 0uy
        if Flags.set MethodImplFlags.Runtime row.ImplFlags then conditions <- conditions + 1uy
        if Flags.set MethodDefFlags.PInvokeImpl row.Flags then conditions <- conditions + 1uy
        conditions = 1uy

[<Struct>]
type private MethodDefRowValidator =
    interface IRowRangeValidator<MethodDefRow> with
        member _.Validate row =
            match row.Flags with
            | _ when row.Flags &&& MethodDefFlags.MemberAccessMask = MethodDefFlags.MemberAccessMask -> // 6
                Some(InvalidMethodAttributesCombination MethodDefFlags.MemberAccessMask :> IValidationError)
            | ValidationResult.CheckFlags InvalidMethodFlags.StaticFinal err // 7a
            | ValidationResult.CheckFlags InvalidMethodFlags.StaticVirtual err // 7b
            | ValidationResult.CheckFlags InvalidMethodFlags.StaticNewSlot err // 7c
            | ValidationResult.CheckFlags InvalidMethodFlags.FinalAbstract err // 7d
            | ValidationResult.CheckFlags InvalidMethodFlags.AbstractPInvokeImpl err // 7e
            | ValidationResult.CheckFlags InvalidMethodFlags.CompilerControlledSpecialName err // 7f
            | ValidationResult.CheckFlags InvalidMethodFlags.CompilerControlledRTSpecialName err // 7g
            | ValidationResult.CheckFlags InvalidMethodFlags.VirtualPInvokeImpl err -> // 24
                Some err
            | _ when
                Flags.anyOfSet
                    (MethodDefFlags.Abstract ||| MethodDefFlags.Final ||| MethodDefFlags.NewSlot ||| MethodDefFlags.Strict)
                    row.Flags
                && not(Flags.set MethodDefFlags.Virtual row.Flags)
                -> // 8, 23
                Some({ MethodMustBeVirtual.Flags = row.Flags } :> IValidationError)
            | _ when Flags.set MethodDefFlags.RTSpecialName row.Flags && not(Flags.set MethodDefFlags.SpecialName row.Flags) -> // 9
                Some(RTSpecialNameMethodMustBeSpecialName() :> IValidationError)
            | _ when not(Flags.set MethodDefFlags.Abstract row.Flags) && not(MethodValidation.nonAbstractMethod &row) -> // 25
                Some({ NonAbstractMethodIsMissingImplementation.Row = row } :> IValidationError)
            | _ when
                row.Flags &&& MethodDefFlags.MemberAccessMask = MethodDefFlags.CompilerControlled
                && not(MethodValidation.compilerControlledMethod &row)
                -> // 26
                Some({ CompilerControlledMethodHasIncorrectImplementation.Row = row } :> IValidationError)
            | _ when row.Rva.IsZero && not(MethodValidation.zeroRvaMethod &row) -> // 33
                Some({ ZeroRvaMethodHasInvalidFlags.Row = row } :> IValidationError)
            | _ -> None
        member _.Duplicate row = { DuplicateMethodDefRow.Row = row } :> IValidationError // 21

[<Sealed>]
type MethodDefTableBuilder internal () =
    // TODO: Make MethodDefRow implement IComparable, since methods marked CompilerControlled are ignored in duplicate checking (18).
    let rows = RangedRowList<MethodDefRow, MethodDefRowValidator>()
    member _.Count = rows.Count
    member _.TryAdd methods: ValidationResult<_> = rows.TryAdd methods
    interface ITableBuilder<MethodDefRow> with
        member this.Count = this.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE row.Rva.Value
            wr.WriteLE(uint16 row.ImplFlags)
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name.Offset
            StreamOffset.writeBlob &wr hsizes row.Signature.MethodDefSig
            TableIndex.write &wr tsizes ValidTableFlags.Param row.ParamList
