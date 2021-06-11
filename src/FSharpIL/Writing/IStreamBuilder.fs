namespace FSharpIL.Writing

open FSharpIL

type internal IStreamBuilder = interface
    abstract StreamLength: uint32 voption
    abstract StreamName: System.Collections.Immutable.ImmutableArray<byte>
    abstract Serialize: builder: byref<ChunkedMemoryBuilder> -> unit
end
