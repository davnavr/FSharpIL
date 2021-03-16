namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text

open Microsoft.FSharp.Core.Printf

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

type MethodCallingConventions =
    | Default
    | VarArg
    // | Generic // of count: int

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
        cconv: MethodCallingConventions,
        retType: ReturnTypeItem,
        parameters: ImmutableArray<ParamItem>
    ) =
    member _.CallingConventions = cconv
    member internal _.Flags =
        let mutable flags =
            match cconv with
            | Default -> CallingConvention.Default
            | VarArg -> CallingConvention.VarArg
        if hasThis then flags <- flags ||| CallingConvention.HasThis
        if explicitThis then flags <- flags ||| CallingConvention.ExplicitThis
        flags
    member _.ReturnType = retType
    member _.Parameters: ImmutableArray<ParamItem> = parameters

    member internal this.CheckOwner owner =
        retType.CheckOwner owner
        for param in this.Parameters do param.CheckOwner owner

// TODO: See if interface for retrieving method signatures is really necessary.
type IMethodDefSignature =
    inherit IIndexValue
    abstract Signature: unit -> MethodDefSignature

/// <summary>Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<Sealed>]
type MethodDefRow internal (body, iflags, attr, name, signature: MethodDefSignature, paramList) =
    member _.Body: IMethodBody = body
    member _.ImplFlags: MethodImplAttributes = iflags
    member _.Flags: MethodAttributes = attr
    member _.Name: Identifier = name
    member _.Signature: MethodDefSignature = signature
    member val ParamList =
        let len = signature.Parameters.Length
        let parameters = ImmutableArray.CreateBuilder<ParamRow> len
        for i = 0 to len - 1 do
            let item = signature.Parameters.Item i
            paramList item i |> parameters.Add
        parameters.ToImmutable()

    member internal _.SkipDuplicateChecking = attr &&& MethodAttributes.MemberAccessMask = MethodAttributes.PrivateScope

    override this.ToString() =
        let visibility =
            match attr ||| MethodAttributes.MemberAccessMask with
            | MethodAttributes.Private -> "private"
            | MethodAttributes.FamANDAssem -> "famandassem"
            | MethodAttributes.Assembly -> "assembly"
            | MethodAttributes.Family -> "family"
            | MethodAttributes.FamORAssem -> "famorassem"
            | MethodAttributes.Public -> "public"
            | _ -> "compilercontrolled"
        let hidebysig = if attr.HasFlag MethodAttributes.HideBySig then " hidebysig" else String.Empty
        let mstatic = if attr.HasFlag MethodAttributes.Static then " static" else String.Empty
        let parameters =
            let builder = StringBuilder()
            let parameters' = signature.Parameters
            for i = 0 to parameters'.Length - 1 do
                let sep = if i < parameters'.Length - 1 then ", " else String.Empty
                bprintf builder "%O %O%s" parameters'.[i] this.ParamList.[i] sep
            builder.ToString()

        // TODO: Add extra flags when printing MethodDef.

        sprintf ".method %s%s%s %O (%s)" visibility hidebysig mstatic signature.ReturnType parameters

    interface IEquatable<MethodDefRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.Name = other.Name && this.Signature = other.Signature

    interface IIndexValue with member this.CheckOwner owner = this.Signature.CheckOwner owner

type MethodDefIndex<'Tag> = TaggedIndex<'Tag, MethodDefRow>

/// <summary>
/// Error used when there is a duplicate row in the <c>MethodDef</c> table (21).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodError (method: MethodDefRow) =
    inherit ValidationError()
    member _.Method = method

[<RequireQualifiedAccess>]
type MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile private () =
    interface IMethodDefSignature with
        member _.CheckOwner _ = ()
        member _.Signature() = failwith "uh oh signature"
