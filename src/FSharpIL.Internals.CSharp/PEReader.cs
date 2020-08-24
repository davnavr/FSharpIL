namespace FSharpIL
{
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

            return error switch
            {
                null => ReadResult.NewOk(result),
                ReadError err => ReadResult.NewError(err),
            };
        }
    }
}
