namespace FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
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
