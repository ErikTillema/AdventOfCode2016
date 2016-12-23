module Problem22b
    
    open Util
    open System
    open System.Collections.Generic

    type Server = | Server of int   // x
                              * int // y
                              * int // free
                              * int // used
    type State = | State of (int * int) * (int * int) // pos empty, pos goal

    let (w,h) = (3,3)
    let serverGrid = Array2D.init w h (fun _ _ -> Server(0,0,0,0))
    let distance = Dictionary<State, int>()
    let dx = [| -1; 1; 0; 0 |]
    let dy = [| 0; 0; -1; 1 |]
    
    let neighbours (x,y) = 
        seq {
            for i in 0..3 do
                let (nx,ny) = (x+dx.[i], y+dy.[i])
                if 0 <= nx && nx < w && 0 <= ny && ny < h then yield (nx,ny)
        }

    let parseServer (line: String) =
        match line with
        | Regex "^/dev/grid/node-x(?<x>\d+)-y(?<y>\d+)\s+\d+T\s+(?<used>\d+)T\s+(?<free>\d+)T\s+\d+%$" [ x; y; used; free ] -> 
            Server(x |> int, y |> int, free |> int, used |> int)
        | _ -> invalidOp "bad input"

    let parseServers() = 
        let sc = Scanner(@"..\..\Problem22.test.in")
        sc.Lines |> Seq.skip 2 |> Seq.map parseServer |> Seq.toArray

    let isStuck pos = 
        let (x,y) = pos
        let (Server(_,_,_,used)) = serverGrid.[x,y]
        used > 20
//        pos |> neighbours |> Seq.forall (fun (nbx,nby) -> 
//                                            let (Server(_,_,nbfree,nbused)) = serverGrid.[nbx,nby] 
//                                            nbfree + nbused < used
//                                        )

    let printGrid() = 
        let printServer (Server(_,_,free,used)) = 
            printf "(%3i %3i)" used (free+used)
        for y in 0..h-1 do
            for x in 0..w-1 do
                let server = serverGrid.[x,y]
                printServer server
                printf " "
            printfn ""

    let bfs() =
        let (Server(sex,sey,_,_)) = serverGrid |> Seq.cast<_> |> Seq.find (fun (Server(_,_,_,used)) -> used = 0)
        let startE = (sex,sey)
        let startG = (w-1,0)
        let startState = State(startE, startG)
        let q = Queue<State>()
        q.Enqueue(startState)
        distance.Add(startState, 0)
        while q.Count > 0 do
            let state = q.Dequeue()
            let (State(posE, posG)) = state
            let d = distance.[state]
            for posnb in (neighbours posE) do
                if not (isStuck posnb) then
                    let newState = 
                        if posnb = posG then State(posG,posE)
                        else State(posnb, posG)
                
                    if not (distance.ContainsKey(newState)) then
                        distance.Add(newState, d+1)
                        q.Enqueue(newState)

    let solve() = 
        let servers = parseServers()
        for server in servers do
            let (Server(x,y,_,_)) = server
            serverGrid.[x,y] <- server
        bfs()
        let result = distance.Keys |> Seq.filter (fun (State(_,posG)) -> posG = (0,0)) |> Seq.map (fun state -> distance.[state]) |> Seq.min
        printfn "%i" result
//        printGrid()
//        let nonStuckServers = servers |> Seq.filter (fun (Server(x,y,_,_)) -> not (isStuck (x,y))) |> Seq.toList
//        let nonStuckServers2 = nonStuckServers |> Seq.filter (fun (Server(_,_,_,used)) -> used > 0) |> Seq.toList
//        let minUsed = nonStuckServers |> Seq.map (fun (Server(_,_,free,used)) -> used) |> Seq.min
//        let minUsedNonZero = nonStuckServers2 |> Seq.map (fun (Server(_,_,free,used)) -> used) |> Seq.min
//        let maxUsed = nonStuckServers |> Seq.map (fun (Server(_,_,free,used)) -> used) |> Seq.max
//        let maxFree = nonStuckServers2 |> Seq.map (fun (Server(_,_,free,used)) -> free) |> Seq.max
//        let minTotal = nonStuckServers |> Seq.map (fun (Server(_,_,free,used)) -> free+used) |> Seq.min
//        let maxTotal = nonStuckServers |> Seq.map (fun (Server(_,_,free,used)) -> free+used) |> Seq.max
