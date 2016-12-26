module Problem24_HeldKarp
    
    open Util
    open System
    open System.Collections.Generic

    let dx = [| -1; 1; 0; 0 |]
    let dy = [| 0; 0; -1; 1 |]

    let (w,h) = (11,5)
    let waypoints = 5
    let waypoint = Array.replicate waypoints (0,0)
    let maze = Array2D.init w h (fun _ _ -> false) // true = open space, false = wall
    let distance = Array2D.init waypoints waypoints (fun _ _ -> Int32.MaxValue)
    let mutable shortestPath = Int32.MaxValue

    let getWaypoint pos =
        if waypoint |> Array.exists ((=) pos) then
            waypoint |> Array.findIndex ((=) pos) |> Option.Some
        else
            None

    let parseInput() =
        let sc = Scanner(@"..\..\Problem24.test.in")
        sc.Lines |> Seq.iteri (fun y line -> 
            let cs = line.ToCharArray()
            cs |> Seq.iteri (fun x c ->
                maze.[x,y] <- (c <> '#')
                if c <> '.' && c <> '#' then
                    let i = (c |> int) - ('0' |> int)
                    waypoint.[i] <- (x,y)
            )
        )

    let bfs k =
        let start = waypoint.[k]
        let q = Queue<_>()
        let dist = Dictionary<int*int,int>()
        q.Enqueue(start)
        dist.Add(start,0)
        while q.Count > 0 do
            let pos = q.Dequeue()
            let (x,y) = pos
            let d = dist.[pos]
            match getWaypoint pos with
            | None -> ()
            | Some(i) -> 
                distance.[k,i] <- d
                distance.[i,k] <- d
            for i in 0..3 do
                let (nx,ny) = ( x+dx.[i] , y+dy.[i] )
                if 0 <= nx && nx < w && 0 <= ny && ny < h then
                    if maze.[nx,ny] && not(dist.ContainsKey(nx,ny)) then
                        dist.Add( (nx,ny), d+1 )
                        q.Enqueue(nx,ny)

    let getSubsets (source: 'a[]) = 
        let rec getSubsets' don acc =
            seq {
                if don = source.Length then
                    yield List.rev acc
                else
                    yield! getSubsets' (don+1) (source.[don] :: acc)
                    yield! getSubsets' (don+1) acc
            }
        getSubsets' 0 []

    // Held Karp
    // O(n^2 * log n * 2^n)
    let getShortestPath() =
        let n = waypoints
        let cost = Dictionary<int*Set<int>,int>()
        let prev = Dictionary<int*Set<int>,int>()

        let subsets = getSubsets [|1..n-1|] |> Seq.sortBy (fun s -> s.Length) |> Seq.toArray |> Array.map (fun arr -> Set<int>(arr))

        subsets |> Array.iteri (fun k subset ->
            for i in 0..n-1 do
                if (i > 0 && not (subset.Contains(i))) || (i=0 && k = subsets.Length-1 ) then
                    let mutable minCost = Int32.MaxValue
                    let mutable minPrev = -1
                    if subset.Count = 0 then
                        minCost <- distance.[0,i]
                    else
                        for j in subset do
                            let trialCost = distance.[j,i] + cost.[(j,subset.Remove(j))] // remove O(log n)
                            if trialCost < minCost then
                                minCost <- trialCost
                                minPrev <- j
                    cost.Add((i, subset), minCost)
                    prev.Add((i, subset), minPrev)
        )
        
        // Silver star:
        let result = seq { 
                        for i in 1..n-1 do
                            let subset = subsets.[subsets.Length-1].Remove(i)
                            yield cost.[(i,subset)]
                     } |> Seq.min
        //let result = cost.[0, subsets.[subsets.Length - 1] ] // Gold star
        result

    let solve() =
        parseInput()
        for i in 0..waypoints-1 do
            bfs i

        let shortestPath = getShortestPath()
        printfn "%i" shortestPath
