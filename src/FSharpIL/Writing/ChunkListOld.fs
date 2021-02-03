namespace rec FSharpIL.Writing

open System.Collections.Generic

[<ReferenceEquality; NoComparison>]
type internal ChunkOld =
    { Data: byte[]
      List: ChunkListOld
      mutable next: ChunkOld option
      mutable previous: ChunkOld option }

    member this.Next =
        if this.List.Tail = Some this
        then None
        else this.next
    member this.Previous =
        if this.List.Head = Some this
        then None
        else this.previous

type internal ChunkListEnumeratorOld =
    struct
        val mutable private current: ChunkOld option
        val mutable private init: bool
        val private list: ChunkListOld

        new(list: ChunkListOld) =
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

        interface IEnumerator<ChunkOld> with
            member this.Current = this.Current
            member this.Current = this.Current :> obj
            member this.MoveNext() = this.MoveNext()
            member this.Reset() = this.Reset()
            member _.Dispose() = ()
    end

[<Sealed>]
type internal ChunkListOld () =
    let sizes = SizeStack()
    let mutable head: ChunkOld option = None
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

    member this.AddAfter(chunk: ChunkOld, data: byte[]) =
        if chunk.List <> this then
            "The chunk must belong to the current list" |> invalidArg (nameof chunk)
        let toAppend = this.CreateChunk(data, chunk.next, Some chunk)
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

    member this.GetEnumerator() = new ChunkListEnumeratorOld(this)

    interface IEnumerable<ChunkOld> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
