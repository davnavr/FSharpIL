﻿namespace FSharpIL.XmlDoc

open FSharpIL.Metadata

type DocMemberTag =
    | Error = 0uy
    | Type = 1uy
    | Field = 2uy
    | Property = 3uy
    | Method = 4uy
    | Event = 5uy

type DocMember = TaggedIndex<DocMemberTag>

[<RequireQualifiedAccess>]
module DocMember =
    // Event|
    let (|Type|Field|Property|Method|) (index: DocMember) =
        match index.Tag with
        | DocMemberTag.Type -> Type(index.ToRawIndex<TypeDefRow>())
        | DocMemberTag.Field -> Field(index.ToRawIndex<FieldRow>())
        | DocMemberTag.Property -> Property(index.ToRawIndex<PropertyRow>())
        | DocMemberTag.Method -> Method(index.ToRawIndex<MethodDefRow>())
        //| DocMemberTag.Event -> Event(index.ToRawIndex<EventRow>())
        | DocMemberTag.Error
        | _ -> invalidArg "index" "Invalid XML documentation member"
    let Type (typeDef: RawIndex<TypeDefRow>) = typeDef.ToTaggedIndex DocMemberTag.Type
    let Field (field: RawIndex<FieldRow>) = field.ToTaggedIndex DocMemberTag.Field
    let Property (property: RawIndex<PropertyRow>) = property.ToTaggedIndex DocMemberTag.Property
    let Method (method: RawIndex<MethodDefRow>) = method.ToTaggedIndex DocMemberTag.Method
    //let Event (event: RawIndex<EventRow>) = event.ToTaggedIndex DocMemberTag.Event

    let allMembers (metadata: CliMetadata) =
        seq {
            for i = 1 to metadata.TypeDef.Count do yield Type(RawIndex i)
            for i = 1 to metadata.Field.Count do yield Field(RawIndex i)
            for i = 1 to metadata.Property.Count do yield Property(RawIndex i)
            for i = 1 to metadata.MethodDef.Count do yield Method(RawIndex i)
            //for i = 1 to metadata.Event.Count do yield Event(RawIndex i)
        }
