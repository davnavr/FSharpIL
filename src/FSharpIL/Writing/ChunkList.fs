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
            { current = list.Head
              init = false
              list = list }

        member this.Current =
            if not this.init then
                invalidOp "The enumerator has either already finished iterating through all chunks, or has not yet started."
            this.current.Value

        member this.MoveNext() =
            match this.current with
            | Some _ when not this.init ->
                this.init <- true
                true
            | Some current' ->
                this.current <- current'.Next
                current'.Next.IsSome
            | _ -> false

        member this.Reset() =
            this.init <- false
            this.current <- this.list.Head

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
    let mutable i = -1

    member _.Count = i + 1

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
        i <- i + 1
        this.Head <- 0u

    member this.Pop() =
        if i < 0 then invalidOp "The size stack was empty."
        let size = this.Head
        i <- i - 1
        if i >= 0 then this.Head <- this.Head + size
        size

[<Sealed>]
type internal ChunkList () =
    let sizes = SizeStack()
    let mutable head: Chunk option = None
    let mutable count = 0u

    member _.Count: uint32 = count
    member _.Head = head
    member _.Tail = Option.bind (fun head' -> head'.previous) head

    member private this.CreateChunk(data: byte[], next, prev) =
        if data.Length <= 0 then
            invalidArg (nameof data) "The chunk data must not be empty."
        { Data = data
          List = this
          next = next
          previous = prev }

    member this.AddAfter(chunk: Chunk, data: byte[]) =
        if chunk.List <> this then
            "The chunk must belong to the current list" |> invalidArg (nameof chunk)
        let toAppend = this.CreateChunk(data, chunk.Next, Some chunk)
        Option.iter (fun next -> next.previous <- Some toAppend) chunk.next
        chunk.next <- Some toAppend
        count <- count + 1u
        this.IncrementSize data.Length
        toAppend

    member this.AddFirst(data: byte[]) =
        let oldHead = head
        let oldTail = this.Tail
        let newHead = this.CreateChunk(data, oldHead, oldTail)
        head <- Some newHead
        if count = 0u then
            newHead.next <- Some newHead
            newHead.previous <- Some newHead
        else
            Option.iter (fun head' -> head'.previous <- Some newHead) oldHead
            Option.iter (fun tail' -> tail'.next <- Some newHead) oldTail
        count <- count + 1u
        this.IncrementSize data.Length
        newHead

    member this.AddLast(data: byte[]) =
        let oldHead = head
        let oldTail = this.Tail
        let newTail = this.CreateChunk(data, oldHead, oldTail)
        if count = 0u then
            head <- Some newTail
            newTail.next <- Some newTail
            newTail.previous <- Some newTail
        else
            Option.iter (fun head' -> head'.previous <- Some newTail) oldHead
            Option.iter (fun tail' -> tail'.next <- Some newTail) oldTail
        count <- count + 1u
        this.IncrementSize data.Length
        newTail

    member _.PushSize() = sizes.Push()
    member _.PopSize() = sizes.Pop()
    member private _.IncrementSize (by: int32) =
        if sizes.Count > 0 then
            sizes.Head <- sizes.Head + uint32 by

    member this.GetEnumerator() = new ChunkListEnumerator(this)

    interface IEnumerable<Chunk> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
