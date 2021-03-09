namespace FSharpIL.Metadata

[<Interface>] type internal IMethodSpec = inherit IIndexValue

[<NoComparison; StructuralEquality>]
type MethodDefOrRef =
    | Def of SimpleIndex<MethodDefRow>
    | RefDefault of MemberRefIndex<MethodRefDefault>
    | RefGeneric of MemberRefIndex<MethodRefGeneric>
    | RefVarArg of MemberRefIndex<MethodRefVarArg>

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | Def method -> IndexOwner.checkIndex owner method
            | RefDefault (SimpleIndex method)
            | RefGeneric (SimpleIndex method)
            | RefVarArg (SimpleIndex method) -> IndexOwner.checkIndex owner method

/// <summary>Represents a row in the <c>MethodSpec</c> table (II.22.29).</summary>
[<System.Runtime.CompilerServices.IsReadOnly>]
[<NoComparison; StructuralEquality>]
type MethodSpecRow = struct
    val Method: MethodDefOrRef
    val internal Item: IMethodSpec // Instantiation
    internal new (method, instantiation) = { Method = method; Item = instantiation }

    member internal this.CheckOwner owner =
        IndexOwner.checkOwner owner this.Method
        IndexOwner.checkOwner owner this.Item

    interface IIndexValue with member this.CheckOwner owner = this.CheckOwner owner
end

/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodSpecError (methodSpec: MethodSpecRow) =
    inherit ValidationError()
    member _.MethodSpec = methodSpec
