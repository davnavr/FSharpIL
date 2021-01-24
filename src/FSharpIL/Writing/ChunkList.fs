namespace rec FSharpIL.Writing

open System.Collections.Generic

[<ReferenceEquality; NoComparison>]
type internal Chunk =
    { Data: byte[]
      List: ChunkList
      mutable next: Chunk option
      mutable previous: Chunk option }

    member this.Next =
        if this.List.Tail = Some this
        then None
        else this.next
    member this.Previous =
        if this.List.Head = Some this
        then None
        else this.previous

type internal ChunkListEnumerator =
    struct
        val mutable private current: Chunk option
        val mutable private init: bool
        val private list: ChunkList

        new(list: ChunkList) =
            { current = None
              init = false
              list = list }

        member this.Current =
            if not this.init then
                invalidOp "The enumerator has either already finished iterating through all chunks, or has not yet started."
            this.current.Value

        member this.MoveNext() =
            match this.current with
            | _ when not this.init ->
                this.init <- true
                this.current <- this.list.Head
                true
            | Some current' ->
                this.current <- current'.Next
                current'.Next.IsSome
            | None -> false

        member this.Reset() =
            this.init <- false
            this.current <- None

        interface IEnumerator<Chunk> with
            member this.Current = this.Current
            member this.Current = this.Current :> obj
            member this.MoveNext() = this.MoveNext()
            member this.Reset() = this.Reset()
            member _.Dispose() = ()
    end

[<Sealed>]
type private SizeStack() =
    let mutable sizes = Array.zeroCreate<uint32> 1
    let mutable i = 0

    member _.Head
        with get() =
            if i < 0 then
                sprintf "Unable to retrieve size, the stack is empty." |> invalidOp
            sizes.[i]
        and set value = sizes.[i] <- value

    member this.Push() =
        if i >= sizes.Length - 1 then
            let old = sizes
            sizes <- Array.zeroCreate<uint32>(sizes.Length * 2)
            Array.blit old 0 sizes 0 old.Length
        else i <- i + 1
        this.Head <- 0u

    member this.Pop() =
        if i < 0 then invalidOp "The size stack was empty."
        let size = this.Head
        i <- i - 1
        size

[<Sealed>]
type internal ChunkList () =
    let sizes = SizeStack()
    let mutable head: Chunk option = None
    let mutable count = 0u

    member _.Count: uint32 = count
    member _.Head = head
    member _.Tail = Option.bind (fun head' -> head'.previous) head

    member this.AddAfter(chunk: Chunk, data: byte[]) =
        if chunk.List <> this then
            "The chunk must belong to the current list" |> invalidArg (nameof chunk)
        let toAppend =
            { Data = data
              List = this
              next = chunk.next
              previous = Some chunk }
        Option.iter (fun next -> next.previous <- Some toAppend) chunk.next
        chunk.next <- Some toAppend
        count <- count + 1u
        this.IncrementSize(uint32 data.Length)
        toAppend

    member private this.AddFirstOrLast(data: byte[]) =
        let toAdd =
            { Data = data
              List = this
              next = head
              previous = this.Tail }
        Option.iter (fun tail' -> tail'.next <- Some toAdd) this.Tail
        Option.iter (fun head' -> head'.previous <- Some toAdd) head
        count <- count + 1u
        this.IncrementSize(uint32 data.Length)
        toAdd

    member this.AddFirst(data: byte[]) =
        let newHead = this.AddFirstOrLast data
        head <- Some newHead
        newHead

    member this.AddLast(data: byte[]) =
        let tail = this.AddFirstOrLast data

        match head with
        | Some head' when head'.Next.IsNone -> head'.next <- Some tail
        | None -> head <- Some tail
        | _ -> ()

        tail

    member _.PushSize() = sizes.Push()
    member _.PopSize() = sizes.Pop()
    member internal _.IncrementSize by = sizes.Head <- sizes.Head + by

    member this.GetEnumerator() = new ChunkListEnumerator(this)

    interface IEnumerable<Chunk> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
