namespace FSharpIL
{
    using System.IO;
    using System.Linq;
    using Microsoft.FSharp.Core;
    using ReadResult = Microsoft.FSharp.Core.FSharpResult<Types.PortableExecutable, Types.ReadError>;
    using static Types;
    using System;

    internal sealed class PEReader : FSharpFunc<Stream, ReadResult>
    {
        private readonly string name;

        internal PEReader(string name)
        {
            this.name = name;
        }

        public override ReadResult Invoke(Stream stream)
        {
            using var source = new ByteStream(this.name, stream);
            var result = new PortableExecutable(DosStub.NewDosStub(0));
            ReadError? error = null;

            DOSHeader(source, ref result, ref error);
            if (error != null) goto result;

            PEFileHeader(source, ref result, ref error);
            if (error != null) goto result;

            result:
            return error switch
            {
                null => ReadResult.NewOk(result),
                ReadError err => ReadResult.NewError(err),
            };
        }

        private static void DOSHeader(ByteStream stream, ref PortableExecutable result, ref ReadError? error)
        {
            switch (stream.ReadBytes(2))
            {
                case null:
                    error = ReadError.NonPEFile;
                    return;
                case byte[] magic when magic[0] == 0x4D && magic[1] == 0x5A:
                    const uint offset = 0x3C;
                    stream.TryMove(offset);
                    var lfanew = stream.ReadUInt32();

                    if (stream.BytesRead != offset + 4 || lfanew is null)
                    {
                        error = ReadError.NewInvalidPESignatureOffset(null);
                        return;
                    }

                    var stub = DosStub.NewDosStub(lfanew.Value);
                    if (lfanew <= stream.BytesRead)
                    {
                        error = ReadError.NewInvalidPESignatureOffset(stub);
                        return;
                    }

                    result = result.SetDosHeader(stub);
                    return;
                case byte[] bytes:
                    error = ReadError.NewIncorrectDOSMagic(bytes[0], bytes[1]);
                    return;
            }
        }

        private static void PEFileHeader(ByteStream stream, ref PortableExecutable result, ref ReadError? error)
        {
            var lfanew = result.DosHeader.lfanew;
            stream.TryMove(lfanew);

            if (stream.BytesRead != lfanew)
            {
                error = ReadError.NewInvalidPESignatureOffset(result.DosHeader);
                return;
            }

            var signature = new byte[] { 0x50, 0x45, 0, 0 }; // PE\0\0
            switch (stream.ReadBytes(4))
            {
                case null:
                    error = ReadError.NewInvalidPESignature(null);
                    return;
                case byte[] invalid when !invalid.SequenceEqual(signature):
                    var bytes = Tuple.Create(invalid[0], invalid[1], invalid[2], invalid[3]);
                    error = ReadError.NewInvalidPESignature(bytes);
                    return;
                default:
                    break;
            }

            // TODO: Read the PE file header.
        }
    }
}
