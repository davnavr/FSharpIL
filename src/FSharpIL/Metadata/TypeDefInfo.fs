namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type DelegateInfo internal
    (
        row: RawIndex<DelegateDef>,
        ctor: RawIndex<ObjectConstructor>,
        invoke: RawIndex<InstanceMethod>,
        beginInvoke: RawIndex<InstanceMethod>,
        endInvoke: RawIndex<InstanceMethod>
    ) =
    member _.Row = row
    member _.Constructor = ctor
    member _.Invoke = invoke
    member _.BeginInvoke = beginInvoke
    member _.EndInvoke = endInvoke

[<IsReadOnly; Struct>]
type EnumValueRow internal (row: RawIndex<StaticField>, value: RawIndex<ConstantRow>) =
    member _.Row = row
    member _.Value = value

[<IsReadOnly; Struct>]
type EnumInfo internal (row: RawIndex<EnumDef>, ivalue: RawIndex<InstanceField>, values: ImmutableArray<EnumValueRow>) =
    member _.Row = row
    /// <summary>Gets the instance field that stores the value of the enumeration, usually named <c>value__</c> (33i).</summary>
    member _.Value = ivalue
    member _.EnumValues = values
