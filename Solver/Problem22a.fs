module Problem22a
    
    open Util
    open System

    type Server = | Server of int   // x
                              * int // y
                              * int // free
                              * int // used

    let parseServer (line: String) =
        match line with
        | Regex "^/dev/grid/node-x(?<x>\d+)-y(?<y>\d+)\s+\d+T\s+(?<used>\d+)T\s+(?<free>\d+)T\s+\d+%$" [ x; y; used; free ] -> 
            Server(x |> int, y |> int, free |> int, used |> int)
        | _ -> invalidOp "bad input"

    let parseServers() = 
        let sc = Scanner(@"..\..\Problem22.test.in")
        sc.Lines |> Seq.skip 2 |> Seq.map parseServer |> Seq.toArray

    let rec binarysearch value (servers: Server[]) start endd = 
        if start >= endd then
            start
        else
            let mid = (endd+start)/2
            let (Server(_,_,free,_)) = servers.[mid]
            if value > free then
                binarysearch value servers (mid+1) endd
            else
                binarysearch value servers start mid

    let countPairs (servers: Server[]) = 
        let n = servers.Length
        let servers = servers |> Array.sortBy (fun (Server(_,_,free,_)) -> free)
        let mutable result = 0
        for i in 0..n-1 do
            let (Server(_,_,_,used)) = servers.[i]
            if used > 0 then
                // binary search on used
                let index = binarysearch used servers 0 n
                let count = n - index + (if i >= index then -1 else 0)
                result <- result + count
        result

    let solve() = 
        let servers = parseServers()
        let result = countPairs servers
        printfn "%i" result
