module Problem24
    
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

    let rec getBestPath don (used: bool[]) totalDistance lastWaypoint =
        if don = waypoints then
            shortestPath <- min shortestPath totalDistance // Silver star
            //shortestPath <- min shortestPath (totalDistance + distance.[lastWaypoint,0]) // Gold star
        else
            for i in 0..waypoints-1 do
                if not used.[i] then
                    // choose next waypoint to be i
                    used.[i] <- true
                    getBestPath (don+1) used (totalDistance + distance.[lastWaypoint,i]) i
                    used.[i] <- false

    let solve() =
        parseInput()
        for i in 0..waypoints-1 do
            bfs i
        let used = Array.replicate waypoints false
        used.[0] <- true
        getBestPath 1 used 0 0
        printfn "%i" shortestPath
