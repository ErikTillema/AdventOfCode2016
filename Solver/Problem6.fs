module Problem6

    open Util
    open System
    open System.Collections.Generic

    let updateStats c (stats: Dictionary<char,int>) = 
        if not (stats.ContainsKey(c)) then stats.Add(c, 0)
        stats.[c] <- stats.[c] + 1

    let updateAllStats (s: String) (allStats: Dictionary<char,int> array) =
        s |> Seq.iteri (fun i c -> updateStats c allStats.[i])

    let getMostFrequestChar (stats: Dictionary<char,int>) =
        stats 
            |> Array.ofSeq |> Array.map (fun kvp -> kvp.Key, kvp.Value)
            |> Array.sortBy (fun (c,count) -> (count, c))  // use -> (-count, c) for problem a,  -> (count, c) for problem b.
            |> Array.map fst |> Array.head

    let getMessage (allStats: Dictionary<char,int> array) =
        let a = allStats |> Array.map getMostFrequestChar
        a |> String

    let solve() =
        let n = 8
        let allStats = Array.init n (fun i -> Dictionary<char,int>())
        let sc = new Scanner(@"..\..\Problem6.in")
        for line in sc.Lines do
            updateAllStats line allStats
        let message = getMessage allStats
        printfn "%s" message
