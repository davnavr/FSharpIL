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

        internal uint BytesRead { get; private set; }

        internal byte? ReadByte()
        {
            switch (this.stream.ReadByte())
            {
                case -1:
                    return null;
                case int value:
                    this.BytesRead += 1;
                    return (byte)value;
            }
        }

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
        internal void TryMove(uint offset)
        {
            while (this.BytesRead < offset)
            {
                switch (this.ReadByte())
                {
                    case null:
                        return;
                    default:
                        continue;
                }
            }
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
                byte[] bytes => bytes[0] + ((uint)bytes[1] << 8) + ((uint)bytes[2] << 16) + ((uint)bytes[3] << 24),
            };
        }

        /// <summary>
        /// Reads an unsigned little-endian 2-byte integer.
        /// </summary>
        /// <returns>The parsed <see cref="ushort"/>, or <see langword="null"/> if not enough bytes were read.</returns>
        internal ushort? ReadUInt16()
        {
            return this.ReadBytes(4) switch
            {
                null => null,
                byte[] bytes => (ushort)(bytes[0] + (bytes[1] << 8)),
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
