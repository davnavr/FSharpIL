(*** hide ***)
#r "../../src/FSharpIL/bin/Release/netstandard2.1/FSharpIL.dll"
(**
# Hello World
*)
open System

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

metadata {
    let! assm =
        setAssembly
            { Name = AssemblyName.ofStr "HelloWorld"
              HashAlgId = ()
              Version = Version()
              Flags = ()
              PublicKey = None
              Culture = NullCulture }
    let! mscorlib =
        referenceAssembly
            { Version = Version(5, 0, 0, 0)
              PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
              Name = AssemblyName.ofStr "System.Private.CoreLib"
              Culture = NullCulture
              HashValue = None }
    let! consolelib =
        referenceAssembly
            { Version = Version(5, 0, 0, 0)
              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
              Name = AssemblyName.ofStr "System.Console"
              Culture = NullCulture
              HashValue = None }

    let! console =
        referenceType
            { TypeName = Identifier.ofStr "Console"
              TypeNamespace = "System"
              ResolutionScope = ResolutionScope.AssemblyRef consolelib }
    let! object =
        referenceType
            { TypeName = Identifier.ofStr "Object"
              TypeNamespace = "System"
              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
    let! tfmAttr =
        referenceType
            { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
              TypeNamespace = "System.Runtime.Versioning"
              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
    let string = ParamItem.create EncodedType.String

    let! writeLine =
        referenceMethod
            { Class = MemberRefParent.TypeRef console
              MemberName = Identifier.ofStr "WriteLine"
              Signature =
                { HasThis = false
                  ExplicitThis = false
                  ReturnType = ReturnType.voidItem
                  Parameters = ImmutableArray.Create string
                  VarArgParameters = ImmutableArray.Empty } }
    let! tfmAttrCtor =
        referenceMethod
            { Class = MemberRefParent.TypeRef tfmAttr
              MemberName = Identifier.ofStr ".ctor"
              Signature =
                { HasThis = true // TODO: Figure out flags.
                  ExplicitThis = false
                  ReturnType = ReturnType.voidItem
                  Parameters = ImmutableArray.Create string
                  VarArgParameters = ImmutableArray.Empty } }

    do!
        { Parent = CustomAttributeParent.Assembly assm
          Type = CustomAttributeType.MemberRef tfmAttrCtor
          Value =
            { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
              NamedArg = ImmutableArray.Empty (* FrameworkDisplayName = "" *) }
            |> Some }
        |> attribute

    let main =
        { Body =
            fun content ->
                let writer = MethodBodyWriter content
                writer.Ldstr "Hello World!"
                writer.Call writeLine
                writer.Ret()
            |> MethodBody.create
          ImplFlags = MethodImplFlags.None
          MethodName = Identifier.ofStr "Main"
          Flags = Flags.staticMethod { Visibility = Public; HideBySig = true }
          Signature =
            let args =
                EncodedType.Array(EncodedType.String, ArrayShape.OneDimension)
                |> ParamItem.create
                |> ImmutableArray.Create
            StaticMethodSignature(MethodCallingConventions.Default, ReturnType.voidItem, args)
          ParamList = fun _ _ -> Param { Flags = ParamFlags.None; ParamName = "args" } }
        |>  StaticClassMethod.Method

    let! programBuilder =
        buildStaticClass
            { Access = TypeVisibility.Public
              ClassName = Identifier.ofStr "Program"
              Extends = Extends.TypeRef object
              Flags = Flags.staticClass ClassFlags.None
              TypeNamespace = "HelloWorld" }

    let! main' = programBuilder.AddMethod main
    let! program = programBuilder.BuildType()

    do! program.Value.MethodList.GetIndex main' |> setEntrypoint
}
|> CliMetadata.createMetadata
    { Name = Identifier.ofStr "HelloWorld.dll"
      Mvid = Guid.NewGuid() }
|> ValidationResult.get
|> PEFile.ofMetadata IsExe
// |> WritePE.toFile

(*** include-output ***)
