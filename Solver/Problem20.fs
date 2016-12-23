module Problem20
    
    open Util
    open System
    open System.Collections.Generic

    let parseLine (line: String) = 
        let ss = line.Split("-".ToCharArray())
        let from = ss.[0] |> int64
        let too = ss.[1] |> int64
        from, too

    let parseLines() =
        let sc = Scanner(@"..\..\Problem20.test.in")
        sc.Lines |> Seq.map parseLine |> Seq.sortBy id |> Seq.toArray

    let rec getAllowedBlocks acc (tail: (int64*int64) option) (blocks: (int64*int64)[]) i =
        let getNewTail (tailFrom, tailToo) (from, too) = 
            let newTailFrom = max tailFrom (too+1L)
            if newTailFrom <= tailToo then
                Some(newTailFrom, tailToo)
            else 
                None

        match tail with
        | None -> List.rev acc
        | Some(tail) ->
            if i >= blocks.Length then
                List.rev (tail::acc)
            else
                let (from, too) = blocks.[i]
                let (tailFrom, tailToo) = tail
                if from > tailFrom then
                    // split tail
                    let allowedBlock = (tailFrom, from-1L)
                    let newTail = getNewTail tail (from,too)
                    getAllowedBlocks (allowedBlock::acc) newTail blocks (i+1)
                else
                    // shorten tail
                    let newTail = getNewTail tail (from,too)
                    getAllowedBlocks acc newTail blocks (i+1)
    
    let solve() = 
        let blocks = parseLines()
        let max = 9L
        let allowedBlocks = getAllowedBlocks [] (Some(0L, max)) blocks 0
        let firstAllowed = fst (allowedBlocks.[0])
        let allowedCount = allowedBlocks |> List.sumBy (fun (from,too) -> too-from+1L) 
        printfn "%i %i" firstAllowed allowedCount
