namespace FSharpIL.Writing

[<System.Obsolete>]
[<Sealed>]
type internal SizeStack() =
    let mutable sizes = Array.zeroCreate<uint32> 1
    let mutable i = -1

    member _.Count = i + 1

    member _.Head
        with get() =
            if i < 0 then
                sprintf "Unable to retrieve size, the stack is empty." |> invalidOp
            sizes.[i]
        and set value =
            if i >= 0 then sizes.[i] <- value

    member this.Push() =
        if i >= sizes.Length - 1 then
            let old = sizes
            sizes <- Array.zeroCreate<uint32>(sizes.Length * 2)
            Array.blit old 0 sizes 0 old.Length
        i <- i + 1
        this.Head <- 0u

    member this.Pop() =
        if i < 0 then invalidOp "The size stack was empty."
        let size = this.Head
        i <- i - 1
        if i >= 0 then this.Head <- this.Head + size
        size

    member this.Increment size = this.Head <- this.Head + size
