namespace FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Compare

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type Member<'Member when 'Member :> IEquatable<'Member>> internal (owner: CliType, mber: 'Member) =
    member _.Owner = owner
    member _.Member = mber

    interface IEquatable<Member<'Member>> with
        member _.Equals other = owner === other.Owner && mber === other.Member

type OwnedField = Member<Field>
