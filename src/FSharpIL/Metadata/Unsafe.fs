/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Unsafe

open System.Collections.Immutable
open System.Reflection

let changeIndexTag<'From, 'To> (index: RawIndex<'From>) = index.ChangeTag<'To>()

let tryAddTypeDefRow<'Tag> (builder: CliMetadataBuilder) flags typeName typeNamespace extends parent =
    let row =
        TypeDefRow (
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        )
    match builder.TypeDef.TryAdd &row with
    | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
    | ValueNone -> DuplicateTypeDefError(row).ToResult()

let tryAddMethodDefRow<'Tag> (builder: CliMetadataBuilder) owner (method: inref<_>) =
    match builder.Method.TryAdd(owner, method) with
    | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
    | ValueNone -> DuplicateMethodError(method).ToResult()

let tryCreateMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList =
    let method =
        MethodDefRow (
            body,
            implFlags,
            methodFlags,
            name,
            signature,
            paramList
        )
    tryAddMethodDefRow<'Tag> builder owner &method

let createMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList =
    tryCreateMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList |> ValidationError.check

/// <param name="builder">The CLI metadata with the <c>TypeDef</c> table that the delegate type will be added to.</param>
/// <param name="del">
/// A <c>TypeRef</c> corresponding to the <see cref="T:System.Delegate"/> or <see cref="T:System.MulticastDelegate"/> type.
/// </param>
/// <param name="asyncResult">A <c>Type</c> corresponding to the <see cref="T:System.IAsyncResult"/> type.</param>
/// <param name="asyncCallback">A <c>Type</c> corresponding to the <see cref="T:System.AsyncCallback"/> type.</param>
/// <param name="def"/>
let tryAddDelegateRow builder del asyncResult asyncCallback (def: inref<DelegateDef>) =
    let result =
        tryAddTypeDefRow<DelegateDef>
            builder
            def.Flags
            def.DelegateName
            def.TypeNamespace
            del
            (TypeVisibility.enclosingClass def.Access)
    match result with
    | Ok del' ->
        let trow = del'.AsTypeIndex()
        let asyncResult' = ParamItem.create asyncResult

        // TODO: Create easier way to make methods and constructors whose implementations are 'runtime'
        let ctor =
            createMethodDefRow<ObjectConstructor>
                builder
                trow
                MethodBody.none
                DelegateHelpers.implFlags
                DelegateHelpers.ctorFlags
                (Identifier.ofStr ".ctor")
                (DelegateHelpers.ctorSignature builder)
                DelegateHelpers.ctorParameters

        let invoke =
            createMethodDefRow<InstanceMethod>
                builder
                trow
                MethodBody.none
                DelegateHelpers.implFlags
                def.MethodFlags
                (Identifier.ofStr "Invoke")
                (DelegateHelpers.invokeSignature builder &def)
                ParamList.noname

        let beginInvoke =
            let pcount = def.Parameters.Length

            let signature =
                let parameters = ImmutableArray.CreateBuilder(pcount + 2)
                parameters.AddRange def.Parameters
                parameters.Add asyncResult' // callback
                parameters.Add(ParamItem.create EncodedType.Object) // objects

                MethodDefSignature(true, false, Default, ReturnType.encoded asyncCallback, parameters.ToImmutable())

            createMethodDefRow<InstanceMethod>
                builder
                trow
                MethodBody.none
                DelegateHelpers.implFlags
                def.MethodFlags
                (Identifier.ofStr "BeginInvoke")
                (builder.Blobs.MethodDefSig.GetOrAdd signature)
                (fun _ i ->
                    let name =
                        if i = pcount
                        then "callback"
                        elif i = pcount + 1
                        then "objects"
                        else ""
                    Param { ParamName = name; Flags = ParamFlags() })

        let endInvoke =
            let signature =
                MethodDefSignature (
                    true,
                    false,
                    Default,
                    def.ReturnType,
                    ImmutableArray.Create asyncResult'
                )
                |> builder.Blobs.MethodDefSig.GetOrAdd
            createMethodDefRow<InstanceMethod>
                builder
                trow
                MethodBody.none
                DelegateHelpers.implFlags
                def.MethodFlags
                (Identifier.ofStr "EndInvoke")
                signature
                (fun _ _ -> Param { ParamName = "result"; Flags = ParamFlags() })

        DelegateInfo(del', ctor, invoke, beginInvoke, endInvoke) |> Ok
    | Error err -> Error err

let inline addDelegateRow builder del asyncResult asyncCallback (def: inref<DelegateDef>) =
    tryAddDelegateRow builder del asyncResult asyncCallback &def |> ValidationError.check
