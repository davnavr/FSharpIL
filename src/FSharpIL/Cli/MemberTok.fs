[<AutoOpen>]
module FSharpIL.Cli.MemberTok

open System
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Compare

[<Struct; NoComparison; CustomEquality>]
type MemberTok<'Member when 'Member :> IEquatable<'Member>> (owner: TypeTok, mber: 'Member) =
    member _.Owner = owner
    member _.Member = mber

    override _.ToString() = String.Concat(owner, "::", mber)

    override _.GetHashCode() = HashCode.Combine(owner, mber)

    interface IEquatable<MemberTok<'Member>> with
        member _.Equals other = owner === other.Owner && mber === other.Member

    override this.Equals obj =
        match obj with
        | :? MemberTok<'Member> as other -> this === other
        | _ -> false

type FieldTok = MemberTok<Field>
type MethodTok = MemberTok<Method>

[<Struct; NoComparison; CustomEquality>]
type MethodTok<'Owner, 'Method when 'Method : not struct and 'Method :> Method> (token: MethodTok) =
    member _.Method = Unsafe.As<'Method> token.Member
    member _.Token = token

    new (owner, method: 'Method) = MethodTok<_, _>(token = MethodTok(owner, method :> Method))

    override _.ToString() = token.ToString()

    override _.GetHashCode() = token.GetHashCode()

    interface IEquatable<MethodTok<'Owner, 'Method>> with member _.Equals other = token === other.Token

    override this.Equals obj =
        match obj with
        | :? MethodTok<'Owner, 'Method> as other -> this === other
        | _ -> false

[<Struct>]
type FieldTok<'Owner, 'Field when 'Field : not struct and 'Field :> Field> (token: FieldTok) =
    member _.Field = Unsafe.As<'Field> token.Member
    member _.Token = token

    new (owner, field: 'Field) = FieldTok<_, _>(token = FieldTok(owner, field))

let inline (|MethodTok|) (method: MethodTok<_, _>) = struct(method.Token.Owner, method.Method)

[<RequireQualifiedAccess>]
module MethodTok =
    let create owner method = MethodTok(owner, method)

    let unsafeAs token = MethodTok<'Owner, 'Method> token

    let ofTypeDef owner method cache =
        MethodTok<DefinedType, 'Method>(TypeTok.Named(NamedTypeCache.addDefined owner cache), method)

    let ofTypeRef owner method cache =
        MethodTok<ReferencedType, 'Method>(TypeTok.Named(NamedTypeCache.addReferenced owner cache), method)

let inline (|FieldTok|) (field: FieldTok<_, _>) = struct(field.Token.Owner, field.Field)

[<RequireQualifiedAccess>]
module FieldTok =
    let create owner field = FieldTok(owner, field)

    let unsafeAs token = FieldTok<'Owner, 'Field> token

    let ofTypeDef owner field cache =
        FieldTok<DefinedType, 'Field>(TypeTok.Named(NamedTypeCache.addDefined owner cache), field)

    let ofTypeRef owner field cache =
        FieldTok<ReferencedType, 'Field>(TypeTok.Named(NamedTypeCache.addReferenced owner cache), field)

type DefinedMethodTok = MethodTok<DefinedType, DefinedMethod>

[<Struct>]
type PropertyTok (owner: NamedType, property: Property) =
    member _.Owner =
        match owner with
        | NamedType.DefinedType defined -> defined
        | NamedType.ReferencedType _ -> invalidOp "Type references cannot own property definitions"

    member inline private _.MethodToken method =
        match method with
        | ValueSome getter -> ValueSome(DefinedMethodTok(TypeTok.Named owner, getter))
        | ValueNone -> ValueNone

    member _.Property = property
    member this.Getter = this.MethodToken property.Getter
    member this.Setter = this.MethodToken property.Setter

module PropertyTok =
    let ofTypeDef owner property cache = PropertyTok(NamedTypeCache.addDefined owner cache, property)

[<Struct>]
type EventTok (owner: NamedType, event: Event) =
    member _.Owner =
        match owner with
        | NamedType.DefinedType defined -> defined
        | NamedType.ReferencedType _ -> invalidOp "Type references cannot own event definitions"

    member inline private _.MethodToken method = DefinedMethodTok(TypeTok.Named owner, method)

    member _.Event = event
    member this.Add = this.MethodToken event.Add
    member this.Remove = this.MethodToken event.Remove
    member this.Raise =
        match event.Raise with
        | ValueSome raise -> ValueSome(this.MethodToken raise)
        | ValueNone -> ValueNone

module EventTok =
    let ofTypeDef owner event cache = EventTok(NamedTypeCache.addDefined owner cache, event)
