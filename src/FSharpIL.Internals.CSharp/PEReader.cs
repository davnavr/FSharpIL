namespace FSharpIL
{
    using System;
    using System.IO;
    using Microsoft.FSharp.Core;
    using ReadResult = Microsoft.FSharp.Core.FSharpResult<Types.PortableExecutable, Types.ReadError>;
    using static Types;

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
            if (error is ReadError err1)
                return ReadResult.NewError(err1);

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
                    // TODO: Move to 0x3C for lfanew.
                    throw new NotImplementedException();
            }
        }
    }
}
