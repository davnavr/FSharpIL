[<AutoOpen>]
module FSharpIL.Cli.MemberTok

open System
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Compare

[<Struct; NoComparison; CustomEquality>]
type MemberTok<'Member when 'Member :> IEquatable<'Member>> (owner: TypeTok, mber: 'Member) =
    member _.Owner = owner
    member _.Member = mber

    interface IEquatable<MemberTok<'Member>> with
        member _.Equals other = owner === other.Owner && mber === other.Member

type FieldTok = MemberTok<Field>
type MethodTok = MemberTok<Method>

[<Struct>]
type MethodTok<'Owner, 'Method when 'Method : not struct and 'Method :> Method> (token: MethodTok) =
    member _.Method = Unsafe.As<'Method> token.Member
    member _.Token = token
    
    new (owner, method: 'Method) = MethodTok<_, _>(token = MethodTok(owner, method))

[<Struct>]
type FieldTok<'Owner, 'Field when 'Field : not struct and 'Field :> Field> (token: FieldTok) =
    member _.Field = Unsafe.As<'Field> token.Member
    member _.Token = token

    new (owner, field: 'Field) = FieldTok<_, _>(token = FieldTok(owner, field))

let inline (|MethodTok|) (method: MethodTok<_, _>) = struct(method.Token.Owner, method.Method)

[<RequireQualifiedAccess>]
module MethodTok =
    let unsafeAs token = MethodTok<'Owner, 'Method> token
    let ofTypeDef owner method cache =
        MethodTok<DefinedType, 'Method>(TypeTok.Named(NamedTypeCache.addDefined owner cache), method)
    let ofTypeRef owner method cache =
        MethodTok<ReferencedType, 'Method>(TypeTok.Named(NamedTypeCache.addReferenced owner cache), method)

let inline (|FieldTok|) (field: FieldTok<_, _>) = struct(field.Token.Owner, field.Field)

[<RequireQualifiedAccess>]
module FieldTok =
    let unsafeAs token = FieldTok<'Owner, 'Field> token
    let ofTypeDef owner field cache =
        FieldTok<DefinedType, 'Field>(TypeTok.Named(NamedTypeCache.addDefined owner cache), field)
    let ofTypeRef owner field cache =
        FieldTok<ReferencedType, 'Field>(TypeTok.Named(NamedTypeCache.addReferenced owner cache), field)
