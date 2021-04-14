/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Unsafe

open System.Collections.Immutable
open System.Reflection

open FSharpIL

let changeIndexTag<'From, 'To> (index: RawIndex<'From>) = index.ChangeTag<'To>()

let createFlags<'Tag, 'Flags when 'Flags :> System.Enum> flags = ValidFlags<'Tag, 'Flags> flags

let tryCreateTypeDefRow<'Tag> (builder: CliMetadataBuilder) flags typeName typeNamespace extends parent =
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

// TODO: Only use inref for MethodDefRow if it is a struct.
let tryAddMethodDefRow<'Tag> (builder: CliMetadataBuilder) owner method =
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
    tryAddMethodDefRow<'Tag> builder owner method

let inline createMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList =
    tryCreateMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList |> ValidationError.check

let tryAddFieldRow<'Tag> (builder: CliMetadataBuilder) owner field =
    match builder.Field.TryAdd(owner, field) with
    | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
    | ValueNone -> DuplicateFieldError(field).ToResult()

let tryAddPropertyRow<'Tag>
    (builder: CliMetadataBuilder)
    (owner: RawIndex<TypeDefRow>)
    (methods: inref<PropertyMethods>)
    (property: inref<PropertyRow>)
    =
    match builder.PropertyMap.TryAdd(owner, property) with
    | ValueSome index ->
        if builder.MethodSemantics.TryAddProperty(index, &methods)
        then Ok index
        else ExistingMethodSemanticsError(index).ToResult()
    | ValueNone -> DuplicatePropertyError(property).ToResult()

// TODO: Use inref for this property, since internal Definition() method is used.
let tryCreatePropertyRow builder owner methods (property: Property<'Tag, 'Signature>) =
    let row = property.Definition()
    tryAddPropertyRow<Property<'Tag, 'Signature>> builder owner &methods &row

let inline createPropertyRow builder owner methods property =
    tryCreatePropertyRow builder owner methods property |> ValidationError.check

let tryAddEventRow<'Tag>
    (builder: CliMetadataBuilder)
    (owner: RawIndex<TypeDefRow>)
    (methods: inref<EventMethods>)
    (event: inref<EventRow>)
    =
    match builder.EventMap.TryAdd(owner, event) with
    | ValueSome index ->
        if builder.MethodSemantics.TryAddEvent(index, &methods)
        then Ok index
        else ExistingMethodSemanticsError(index).ToResult()
    | ValueNone -> DuplicateEventError(event).ToResult()

let tryCreateEventRow builder owner (methods: inref<_>) (event: inref<Event<'Tag>>) =
    let row = event.Definition()
    tryAddEventRow<Event<'Tag>> builder owner &methods &row

let inline createEventRow builder owner methods event =
    tryCreateEventRow builder owner &methods &event |> ValidationError.check

/// <summary>Creates an event with <c>.addon</c> and <c>.removeon</c> methods and corresponding private field.</summary>
let tryCreateEvent<'Tag, 'Body when 'Body :> IMethodBody>
    (builder: CliMetadataBuilder)
    (owner: RawIndex<TypeDefRow>)
    (event: inref<Event<'Tag>>)
    methodFlags
    hasThis
    (addBody: _ -> 'Body)
    (removeBody: _ -> 'Body)
    =
    let etype = EventHelpers.eventType event.EventType
    let methodFlags' = methodFlags ||| MethodAttributes.SpecialName

    let signature =
        MethodDefSignature (
            hasThis,
            false,
            MethodCallingConventions.Default,
            ReturnType.itemVoid,
            ImmutableArray.Create(ParamItem.create etype)
        )
        |> builder.Blobs.MethodDefSig.GetOrAdd

    let add =
        tryCreateMethodDefRow<MethodDefRow>
            builder
            owner
            (addBody event.EventType)
            MethodImplAttributes.IL
            methodFlags'
            (Identifier.concat2 Identifier.eventAdd event.EventName)
            signature
            EventHelpers.parameters

    let remove =
        tryCreateMethodDefRow<MethodDefRow>
            builder
            owner
            (removeBody event.EventType)
            MethodImplAttributes.IL
            methodFlags'
            (Identifier.concat2 Identifier.eventRemove event.EventName)
            signature
            EventHelpers.parameters

    match add, remove with
    | Ok add', Ok remove' ->
        let methods = EventMethods(add', remove', ValueNone, ImmutableArray.Empty)
        match tryCreateEventRow builder owner &methods &event with
        | Ok row -> GeneratedEvent(row, add', remove') |> Ok
        | Error err -> Error err
    | Error err, _
    | _, Error err -> Error err

/// <param name="builder">The CLI metadata with the <c>TypeDef</c> table that the delegate type will be added to.</param>
/// <param name="del">Corresponds to the <see cref="T:System.Delegate"/> or <see cref="T:System.MulticastDelegate"/> type.</param>
/// <param name="asyncResult">A <c>Type</c> corresponding to the <see cref="T:System.IAsyncResult"/> type.</param>
/// <param name="asyncCallback">A <c>Type</c> corresponding to the <see cref="T:System.AsyncCallback"/> type.</param>
/// <param name="def"/>
let tryAddDelegateRow builder del asyncResult asyncCallback (def: inref<DelegateDef>) =
    let result =
        tryCreateTypeDefRow<DelegateDef>
            builder
            def.Flags
            def.DelegateName
            def.TypeNamespace
            del
            (TypeVisibility.enclosingClass def.Access)
    match result with
    | Ok del' ->
        let trow = del'.ChangeTag<TypeDefRow>()
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

/// <param name="builder"/>
/// <param name="enum">Corresponds to the <see cref="T:System.Enum"/> type.</param>
/// <param name="def"/>
let tryAddEnumRow builder enum (def: inref<EnumDef>) =
    let result =
        tryCreateTypeDefRow<EnumDef>
            builder
            def.Flags
            def.EnumName
            def.TypeNamespace
            enum
            (TypeVisibility.enclosingClass def.Access)
    match result with
    | Ok enum' ->
        let trow = enum'.ChangeTag<TypeDefRow>()
        let values = ImmutableArray.CreateBuilder def.Values.Count

        let ivalue =
            let vtype =
                match def.Values.UnderlyingType with
                | IntegerType.Bool -> EncodedType.Boolean
                | IntegerType.Char -> EncodedType.Char
                | IntegerType.I1 -> EncodedType.I1
                | IntegerType.U1 -> EncodedType.U1
                | IntegerType.I2 -> EncodedType.I2
                | IntegerType.U2 -> EncodedType.U2
                | IntegerType.I4 -> EncodedType.I4
                | IntegerType.U4 -> EncodedType.U4
                | IntegerType.I8 -> EncodedType.I8
                | IntegerType.U8 -> EncodedType.U8
                | _ -> sprintf "Invalid underlying type for enumeration \"%O\"" def.EnumName |> invalidArg "enumDef"
            let signature =
                FieldSignature(ImmutableArray.Empty, vtype)
                |> builder.Blobs.FieldSig.TryAdd
                |> Result.any
            let row =
                FieldRow (
                    FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName ||| FieldAttributes.Public,
                    Identifier.ofStr "value__",
                    signature
                )
            builder.Field.TryAdd(trow, row).Value.ChangeTag<InstanceField>()

        for value in def.Values do
            let vtype = TypeDefOrRefOrSpecEncoded.TypeDef trow |> EncodedType.ValueType
            let signature =
                FieldSignature(ImmutableArray.Empty, vtype)
                |> builder.Blobs.FieldSig.TryAdd
                |> Result.any
            let field =
                FieldRow (
                    FieldAttributes.HasDefault ||| FieldAttributes.Static ||| FieldAttributes.Literal,
                    value.Name,
                    signature
                )
            let field' = builder.Field.TryAdd(trow, field).Value
            let cvalue = builder.Constant.TryAdd(field', ConstantBlob.Integer value.Value).Value
            values.Add(EnumValueRow(field'.ChangeTag<StaticField>(), cvalue))

        EnumInfo(enum', ivalue, values.ToImmutable()) |> Ok
    | Error err -> Error err

let inline addEnumRow builder enum (def: EnumDef) = tryAddEnumRow builder enum &def |> ValidationError.check

/// <param name="builder"/>
/// <param name="valueType">Corresponds to the <see cref="T:System.ValueType"/> type.</param>
/// <param name="def"/>
let inline tryAddStructRow builder valueType (def: StructDef) =
    tryCreateTypeDefRow<StructDef>
        builder
        (def.Flags.Value ||| def.Access.Tag)
        def.StructName
        def.TypeNamespace
        valueType
        (TypeVisibility.enclosingClass def.Access)

let inline addStructRow builder valueType def = tryAddStructRow builder valueType def |> ValidationError.check
