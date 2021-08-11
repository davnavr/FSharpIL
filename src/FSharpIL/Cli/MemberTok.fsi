[<AutoOpen>]
module FSharpIL.Cli.MemberTok

open System
open System.Runtime.CompilerServices

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type MemberTok<'Member when 'Member :> IEquatable<'Member>> =
    member Owner: TypeTok
    member Member: 'Member

    override ToString: unit -> string

    interface IEquatable<MemberTok<'Member>>

    override GetHashCode: unit -> int32
    override Equals: obj -> bool

type FieldTok = MemberTok<Field>
type MethodTok = MemberTok<Method>

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type MethodTok<'Owner, 'Method when 'Method : not struct and 'Method :> Method> =
    member Method: 'Method
    member Token: MethodTok

    override ToString: unit -> string

    interface IEquatable<MethodTok<'Owner, 'Method>>

    override GetHashCode: unit -> int32
    override Equals: obj -> bool

[<IsReadOnly; Struct; NoComparison; StructuralEquality>] // TODO: Consider using CustomEquality for FieldTok as well if CustomEquality for MethodTok`2 improved performance.
type FieldTok<'Owner, 'Field when 'Field : not struct and 'Field :> Field> =
    member Field: 'Field
    member Token: FieldTok

    interface IEquatable<FieldTok<'Owner, 'Field>>

val inline internal (|MethodTok|) : method: MethodTok<'Owner, 'Method> -> struct(TypeTok * 'Method)

[<RequireQualifiedAccess>]
module MethodTok =
    val internal create : owner: TypeTok -> method: Method -> MethodTok

    val internal unsafeAs : token: MethodTok -> MethodTok<'Owner, 'Method>

    val internal ofTypeDef :
        owner: DefinedType ->
        method: 'Method ->
        cache: NamedTypeCache ->
        MethodTok<DefinedType, 'Method>

    val internal ofTypeRef :
        owner: ReferencedType ->
        method: 'Method ->
        cache: NamedTypeCache ->
        MethodTok<ReferencedType, 'Method>

val inline internal (|FieldTok|) : field: FieldTok<'Owner, 'Field> -> struct(TypeTok * 'Field)

[<RequireQualifiedAccess>]
module FieldTok =
    val internal create : owner: TypeTok -> field: Field -> FieldTok

    val internal unsafeAs : token: FieldTok -> FieldTok<'Owner, 'Method>

    val internal ofTypeDef : owner: DefinedType -> field: 'Field -> cache: NamedTypeCache -> FieldTok<DefinedType, 'Field>

    val internal ofTypeRef :
        owner: ReferencedType ->
        field: 'Field ->
        cache: NamedTypeCache ->
        FieldTok<ReferencedType, 'Field>

type DefinedMethodTok = MethodTok<DefinedType, DefinedMethod>

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type PropertyTok =
    member Owner: DefinedType
    member Property: Property

    member Getter: DefinedMethodTok voption
    member Setter: DefinedMethodTok voption
    //member Other: seq<DefinedMethodTok>

    interface IEquatable<PropertyTok>

[<RequireQualifiedAccess>]
module PropertyTok =
    val internal ofTypeDef : owner: DefinedType -> property: Property -> cache: NamedTypeCache -> PropertyTok

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type EventTok =
    member Owner: DefinedType
    member Event: Event

    member Add: DefinedMethodTok
    member Remove: DefinedMethodTok
    member Raise: DefinedMethodTok voption
    //member Other: seq<DefinedMethodTok>

    interface IEquatable<EventTok>

[<RequireQualifiedAccess>]
module EventTok =
    val internal ofTypeDef : owner: DefinedType -> event: Event -> cache: NamedTypeCache -> EventTok
