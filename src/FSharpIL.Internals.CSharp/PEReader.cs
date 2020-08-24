namespace FSharpIL
{
    using System.IO;
    using Microsoft.FSharp.Core;
    using ReadResult = Microsoft.FSharp.Core.FSharpResult<Types.PortableExecutable, Types.ReadError>;

    internal sealed class PEReader : FSharpFunc<Stream, ReadResult>
    {
        private readonly string name;

        internal PEReader(string name)
        {
            this.name = name;
        }

        public override ReadResult Invoke(Stream stream)
        {
            throw new System.NotImplementedException();
        }
    }
}
