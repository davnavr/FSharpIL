namespace FSharpIL
{
    using System;
    using System.IO;

    internal class ByteStream : IDisposable
    {
        private readonly Stream stream;
        private bool disposed = false;

        internal ByteStream(string name, Stream stream)
        {
            this.stream = stream;
            this.Name = name;
        }

        internal string Name { get; }

        internal uint Position { get; private set; }

        internal byte? ReadByte() =>
            this.stream.ReadByte() switch
            {
                -1 => null,
                int value => (byte)value
            };

        public void Dispose()
        {
            if (!this.disposed)
            {
                this.disposed = true;
                this.stream.Close();
            }
        }
    }
}
