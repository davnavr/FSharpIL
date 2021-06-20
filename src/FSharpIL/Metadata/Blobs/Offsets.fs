﻿namespace FSharpIL.Metadata.Blobs

open System.Runtime.CompilerServices

open FSharpIL.Metadata

/// <summary>An offset into the <c>#Blob</c> heap pointing to a <c>FieldSig</c> (II.23.2.4).</summary>
type [<IsReadOnly; Struct>] FieldSigOffset = internal { FieldSig: BlobOffset }

/// <summary>
/// An offset into the <c>#Blob</c> heap pointing to a <c>MethodDefSig</c>, which describes the return type and parameter types
/// of a method (II.23.2.1).
/// </summary>
type [<IsReadOnly; Struct>] MethodDefSigOffset = internal { MethodDefSig: BlobOffset }

/// <summary>
/// An offset into the <c>#Blob</c> heap pointing to a <c>MethodRefSig</c> (II.23.2.2) or a <c>FieldSig</c> (II.23.2.4).
/// </summary>
type [<IsReadOnly; Struct>] MemberRefSigOffset = internal { MemberRefSig: BlobOffset }

/// <summary>An offset into the <c>#Blob</c> heap pointing to a constant value (II.22.9).</summary>
type [<IsReadOnly; Struct>] ConstantOffset = internal { Constant: BlobOffset }

/// <summary>An offset into the <c>#Blob</c> heap pointing to a <c>CustomAttrib</c> item (II.23.3).</summary>
type [<IsReadOnly; Struct>] CustomAttributeOffset = internal { CustomAttrib: BlobOffset }

/// <summary>
/// An offset into the <c>#Blob</c> heap pointing to a <c>PropertySig</c> item, which describes the type of a property and its
/// parameters (II.23.2.5).
/// </summary>
type [<IsReadOnly; Struct>] PropertySigOffset = internal { PropertySig: BlobOffset }

/// <summary>
/// An offset into the <c>#Blob</c> heap pointing to a <c>MethodSpec</c> item, which describes the generic arguments of a method
/// (II.23.2.15).
/// </summary>
type [<IsReadOnly; Struct>] MethodSpecOffset = internal { MethodSpec: BlobOffset }

/// <summary>
/// An offset into the <c>#Blob</c> heap pointing to a <c>LocalVarSig</c> item, which describes the types of the local variables
/// of a method (II.23.2.6).
/// </summary>
[<IsReadOnly; Struct>]
type LocalVarSigOffset =
    internal { LocalVarSig: BlobOffset }
    member this.IsNull = this.LocalVarSig.BlobOffset = 0u

[<IsReadOnly; Struct>]
type PublicKeyOrToken =
    { IsPublicKey: bool
      Token: BlobOffset }
    member this.IsNull = this.Token.BlobOffset = 0u

[<RequireQualifiedAccess>]
module PublicKeyOrToken =
    let PublicKey offset = { IsPublicKey = true; Token = offset }
    let Token offset = { IsPublicKey = false; Token = offset }
    let Null = Token Unchecked.defaultof<BlobOffset>

    let inline (|Null|Token|PublicKey|) value =
        match value with
        | { Token = BlobOffset.IsZero } -> Null
        | { IsPublicKey = false; Token = offset } -> Token offset
        | { IsPublicKey = true; Token = offset } -> PublicKey offset
