namespace FSharpIL.Writing.Cil

open FSharpIL

type MethodBodyBuilder = struct
    val mutable opcodes: ChunkedMemoryBuilder
    //val targets
end
