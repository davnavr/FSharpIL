namespace FSharpIL.Writing

type IStreamBuilder = interface
    abstract StreamLength: uint32
    abstract Serialize: builder: byref<FSharpIL.ChunkedMemoryBuilder> -> unit
end
