module Problem18
    
    open System
    open System.Collections.Generic
    
    let getNextRow (row: bool[]) = 
        let n = row.Length
        let isTrap l c r = 
            not(l) && not(c) && r 
            || not(c) && not(r) && l
            || not(l) && c && r
            || not(r) && c && l
        let getIsSafe i =
            if i < 0 then true
            elif i >= n then true
            else row.[i]
        let result = Array.replicate n false
        for i in 0..n-1 do
            result.[i] <- isTrap (getIsSafe (i-1)) (getIsSafe i) (getIsSafe (i+1)) |> not
        result

    let getAllRows (firstRow: bool[]) n = 
        let rec getAllRows' acc addCount = 
            match addCount with
            | 0 -> List.rev acc
            | _ -> 
                let next = getNextRow acc.[0]
                getAllRows' (next::acc) (addCount-1)
        getAllRows' [firstRow] (n-1)

    let countSafe (rows: bool[] list)  = 
        let countSafe' (row: bool[]) = row |> Seq.filter id |> Seq.length
        rows |> List.sumBy countSafe' 

    let solve() = 
        let input = "..^^." 
        let n = 3
        let firstRow = input |> Seq.map ((=) '.') |> Seq.toArray
        let rows = getAllRows firstRow n
        let result = rows |> countSafe
        printfn "%i" result

