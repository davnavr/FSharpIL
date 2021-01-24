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
type internal ChunkList () =
    let sizes = System.Collections.Generic.Stack<uint32>()
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

    member _.PushSize() = sizes.Push 0u
    member this.PopSize() =
        let size = sizes.Pop()
        this.IncrementSize size
        size
    member internal _.IncrementSize by =
        // TODO: Figure out how to increment size without always having to pop and push.
        if sizes.Count > 0 then
            let size = sizes.Pop() + by
            sizes.Push size

    member this.GetEnumerator() = new ChunkListEnumerator(this)

    interface IEnumerable<Chunk> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
