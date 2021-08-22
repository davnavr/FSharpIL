namespace FSharpIL.Writing

open FSharpIL
open FSharpIL.PortableExecutable

type internal IStreamBuilder = interface
    abstract StreamLength: uint32 voption
    abstract StreamName: System.Collections.Immutable.ImmutableArray<byte>
    abstract Serialize: builder: byref<ChunkedMemoryBuilder> * methodBodiesRva: Rva * embeddedDataRva: Rva -> unit
end
