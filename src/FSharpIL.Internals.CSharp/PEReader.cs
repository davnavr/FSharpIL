namespace FSharpIL
{
    using System.IO;
    using System.Linq;
    using ReadResult = Microsoft.FSharp.Core.FSharpResult<Types.PortableExecutable, Types.ReadError>;
    using static Types;
    using System;

    internal sealed class PEReader
    {
        private readonly string name;

        internal PEReader(string name)
        {
            this.name = name;
        }

        public ReadResult Read(Stream stream)
        {
            using var source = new ByteStream(this.name, stream);
            var result = new PortableExecutable(DosStub.NewDosStub(0), null);
            ReadError? error = null;

            DOSHeader(source, ref result, ref error);
            if (error != null) goto result; // TODO: See if all of this boilerplate can be removed.

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
                    stream.ToOffset(offset);
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
            stream.ToOffset(lfanew);

            if (stream.BytesRead != lfanew)
            {
                error = ReadError.NewInvalidPESignatureOffset(result.DosHeader);
                return;
            }

            // Magic
            switch (stream.ReadBytes(4))
            {
                case null:
                    error = ReadError.NewInvalidPESignature(null);
                    return;
                case byte[] invalid when !invalid.SequenceEqual(new byte[] { 0x50, 0x45, 0, 0 }):
                    var bytes = Tuple.Create(invalid[0], invalid[1], invalid[2], invalid[3]);
                    error = ReadError.NewInvalidPESignature(bytes);
                    return;
                default:
                    break;
            }

            // II.25.2.2
            var machine = stream.ReadUInt16();
            if (machine is null)
            {
                error = ReadError.NewMissingField("Machine", 2);
                return;
            }

            var sections = stream.ReadUInt16();
            if (sections is null)
            {
                error = ReadError.NewMissingField("NumberOfSections", 2);
                return;
            }

            var time = stream.ReadUInt32();
            if (time is null)
            {
                error = ReadError.NewMissingField("TimeDateStamp", 4);
                return;
            }

            var symbolsPointer = stream.ReadUInt32();
            if (symbolsPointer is null)
            {
                error = ReadError.NewMissingField("PointerToSymbolTable", 4);
                return;
            }

            var symbolsCount = stream.ReadUInt32();
            if (symbolsCount is null)
            {
                error = ReadError.NewMissingField("NumberOfSymbols ", 4);
                return;
            }

            var headerSize = stream.ReadUInt16();
            if (headerSize is null)
            {
                error = ReadError.NewMissingField("SizeOfOptionalHeader", 2);
                return;
            }

            // TODO: Maybe make an enum or validate these values if necessary?
            // II.25.2.2.1
            var characteristics1 = stream.ReadUInt16();
            if (characteristics1 is null)
            {
                error = ReadError.NewMissingField("Characteristics", 2);
                return;
            }

            PEFileHeaderCharacteristics flags;
            switch (stream.ReadUInt16())
            {
                case null:
                    error = ReadError.NewMissingField("Characteristics", 2);
                    return;
                case ushort value:
                    flags = (PEFileHeaderCharacteristics)value;
                    break;
            }

            var header =
                new PEFileHeader(
                        machine.Value,
                        sections.Value,
                        time.Value,
                        symbolsPointer.Value,
                        symbolsCount.Value,
                        headerSize.Value,
                        flags
                    );
            result = result.SetPEFileHeader(header);
        }
    }
}
