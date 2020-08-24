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

        internal byte[]? ReadBytes(int count)
        {
            var results = new byte[count];

            for (int i = 0; i < count; i++)
            {
                switch (this.ReadByte())
                {
                    case null:
                        return null;
                    case var value:
                        results[i] = value.Value;
                        continue;
                }
            }

            return results;
        }

        /// <summary>
        /// Attempts to skip to the specified <paramref name="offset"/>.
        /// </summary>
        /// <param name="offset">The offset from the beginning of the stream to skip to.</param>
        /// <returns><see langword="null"/> if successful, otherwise the current position or <c>0</c> if the offset is less than the current position.</returns>
        internal uint? TryMove(uint offset)
        {
            if (offset < this.Position)
                return 0;

            while (this.Position < offset)
            {
                switch (this.ReadByte())
                {
                    case null:
                        return this.Position;
                    default:
                        this.Position += 1;
                        continue;
                }
            }

            return null;
        }

        /// <summary>
        /// Reads an unsigned little-endian 4-byte integer.
        /// </summary>
        /// <returns>The parsed <see cref="uint"/>, or <see langword="null"/> if not enough bytes were read.</returns>
        internal uint? ReadUInt32()
        {
            return this.ReadBytes(4) switch
            {
                null => null,
                byte[] bytes => bytes[1] + ((uint)bytes[2] << 8) + ((uint)bytes[3] << 16) + ((uint)bytes[4] << 24),
            };
        }

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
