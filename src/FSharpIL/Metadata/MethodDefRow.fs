namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

// NOTE: For methods, SpecialName has to be set if RTSpecialName is set.
// NOTE: For methods, RTSpecialName and SpecialName is set when it is a ctor or cctor

[<IsReadOnly>]
type MethodImplFlags = struct
    val Value: MethodImplAttributes

    new
        (
            [<Optional; DefaultParameterValue(false)>] forwardRef,
            [<Optional; DefaultParameterValue(false)>] preserveSig,
            [<Optional; DefaultParameterValue(false)>] noInlining,
            [<Optional; DefaultParameterValue(false)>] noOptimization
        ) =
        let mutable flags = enum<MethodImplAttributes> 0
        if forwardRef then flags <- flags ||| MethodImplAttributes.ForwardRef
        if preserveSig then flags <- flags ||| MethodImplAttributes.PreserveSig
        if noInlining then flags <- flags ||| MethodImplAttributes.NoInlining
        if noOptimization then flags <- flags ||| MethodImplAttributes.NoOptimization
        { Value = flags }

    interface IFlags<MethodImplAttributes> with member this.Value = this.Value
end

[<IsReadOnly; Struct>]
type MethodCallingConventions =
    | Default
    | VarArg
    | Generic of count: uint32

[<Flags>]
type internal CallingConvention =
    | HasThis = 0x20uy
    | ExplicitThis = 0x40uy
    | Default = 0uy
    | VarArg = 0x5uy
    | Generic = 0x10uy

/// <summary>Represents a <c>MethodDefSig</c>, which captures the signature of a method or global function (II.23.2.1).</summary>
// TODO: Ensure that HasThis is set when ExplicitThis is set (31).
[<IsReadOnly; Struct>]
type MethodDefSignature internal
    (
        hasThis: bool,
        explicitThis: bool,
        cconventions: MethodCallingConventions,
        retType: ReturnTypeItem,
        parameters: ImmutableArray<ParamItem>
    ) =
    member _.HasThis = hasThis
    member _.ExplicitThis = explicitThis
    member _.CallingConventions = cconventions
    member _.ReturnType = retType
    member _.Parameters: ImmutableArray<ParamItem> = parameters

// TODO: For paramList function, add additional information such as number of total arguments.
/// <summary>Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<Sealed>]
type MethodDefRow internal (body, iflags, attr, name, signature) =
    member _.Body: IMethodBody = body
    member _.ImplFlags: MethodImplAttributes = iflags
    member _.Flags: MethodAttributes = attr
    member _.Name: Identifier = name
    member _.Signature: Blob<MethodDefSignature> = signature

    member internal _.SkipDuplicateChecking = attr &&& MethodAttributes.MemberAccessMask = MethodAttributes.PrivateScope

    interface IEquatable<MethodDefRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.Name = other.Name && this.Signature = other.Signature

/// <summary>
/// Error used when there is a duplicate row in the <c>MethodDef</c> table (21).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodError (method: MethodDefRow) =
    inherit ValidationError()
    member _.Method = method
