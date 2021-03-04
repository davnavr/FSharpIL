namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

// NOTE: For methods, SpecialName has to be set if RTSpecialName is set.
// NOTE: For methods, RTSpecialName and SpecialName is set when it is a ctor or cctor

[<IsReadOnly; Struct>]
type MethodImplFlags =
    { ForwardRef: bool
      PreserveSig: bool
      NoInlining: bool
      NoOptimization: bool }

    member this.Value =
        let mutable flags = enum<MethodImplAttributes> 0
        if this.ForwardRef then flags <- flags ||| MethodImplAttributes.ForwardRef
        if this.PreserveSig then flags <- flags ||| MethodImplAttributes.PreserveSig
        if this.NoInlining then flags <- flags ||| MethodImplAttributes.NoInlining
        if this.NoOptimization then flags <- flags ||| MethodImplAttributes.NoOptimization
        flags

    interface IFlags<MethodImplAttributes> with member this.Value = this.Value

    static member None =
        { ForwardRef = false
          PreserveSig = false
          NoInlining = false
          NoOptimization = false }

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
[<IsReadOnly; Struct>]
type MethodDefSignature internal (hasThis: bool, explicitThis: bool, cconv: MethodCallingConventions, retType: ReturnTypeItem, parameters: ImmutableArray<ParamItem>) =
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

type IMethodDefSignature =
    inherit IIndexValue
    abstract Signature: unit -> MethodDefSignature

// TODO: Rename to MethodDefRow
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

    interface IEquatable<MethodDefRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else this.Name = other.Name && this.Signature = other.Signature

    interface IIndexValue with member this.CheckOwner owner = this.Signature.CheckOwner owner

type MethodIndex<'Tag> = TaggedIndex<'Tag, MethodDefRow>

/// <summary>
/// Error used when there is a duplicate row in the <c>MethodDef</c> table (21).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodError (method: MethodDefRow) =
    inherit ValidationError()
    member _.Method = method

type IMethod<'Parent> =
    inherit IIndexValue
    abstract Definition : unit -> MethodDefRow

[<RequireQualifiedAccess>]
type MethodSignatureThatIsAVeryTemporaryValueToGetThingsToCompile private () =
    interface IMethodDefSignature with
        member _.CheckOwner _ = ()
        member _.Signature() = failwith "uh oh signature"

[<AutoOpen>]
module internal MethodHelpers =
    let inline (|MethodDef|) (mthd: #IMethod<_>) = mthd.Definition()
