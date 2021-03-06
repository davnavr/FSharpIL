﻿[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.CustomAttribute

/// <summary>Adds a row to the <c>CustomAttribute</c> table (II.22.10).</summary>
let inline addRow (builder: CliMetadataBuilder) attribute = builder.CustomAttribute.Add &attribute

let inline createRow builder parent ctor value = addRow builder { Parent = parent; Type = ctor; Value = value }
