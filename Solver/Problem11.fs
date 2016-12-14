module Problem11

    open Util
    open System
    open System.Collections.Generic

    let floors = 4
    let types = 2
    let items = types * 2
    
    let distance = Dictionary<int,int>()

    type State = State of (int*int)[] * int

    let initialState = State( [| (1,0); (2,0) |], 0)

    let mutable found = false
    let mutable edges = 0
    let mutable highestDist = 0

    let getIndex (State(state, e)) = 
        let mutable result = 0
        let mutable multiple = 1
        for i in 0..types-1 do
            let g,m = state.[i]
            result <- result + multiple * (floors*g + m)
            multiple <- multiple * floors * floors
        result * floors + e

    let getItems (State(state, _)) floor = 
        seq {
            for i in 0..types-1 do
                let g,m = state.[i]
                if g = floor then yield 2*i
                if m = floor then yield 2*i+1
        } 

    let getGenerators (State(state, _)) floor = 
        seq {
            for i in 0..types-1 do
                let g,_ = state.[i]
                if g = floor then yield i
        } 

    let getMicrochips (State(state, _)) floor = 
        seq {
            for i in 0..types-1 do
                let _,m = state.[i]
                if m = floor then yield i
        }

    let isCompatible state floor = 
        let generators = getGenerators state floor |> Set.ofSeq
        let microchips = getMicrochips state floor |> Array.ofSeq
        generators.Count = 0 || (microchips |> Array.forall generators.Contains)

    let isGenerator i = i%2 = 0
    let isMicrochip i = i%2 <> 0

    let moveItem item fromLevel toLevel (state: (int*int)[]) =
        let typ = item/2
        let g,m = state.[typ]
        if isGenerator item then
            state.[typ] <- (toLevel,m)
        else
            state.[typ] <- (g,toLevel)

    let move1Item item fromLevel toLevel (State(state, _)) =
        let state = Array.copy state
        moveItem item fromLevel toLevel state
        let state = Array.sort state
        State(state,toLevel)

    let move2Items item1 item2 fromLevel toLevel (State(state, _)) =
        let state = Array.copy state
        moveItem item1 fromLevel toLevel state
        moveItem item2 fromLevel toLevel state
        let state = Array.sort state
        State(state,toLevel)

    let take2 (items: int array) = 
        let n = items.Length
        seq {
            for i in 0..n-1 do
                for j in i+1..n-1 do
                    let skip = isGenerator i && isMicrochip j && not (i+1 = j) // don't take a generator + microchip of different types
                    if not skip then
                        yield items.[i], items.[j]
        }

    // from any state we can go to another state by taking the elevator one floor up or one floor down
    // we can only do this is such a way that what remains on the floor is compatible with each other
    // and what will be on the destination floor is compatible with each other
    let getNeighbours state = 
        let getOtherLevels elevatorLevel =
            seq {
                if elevatorLevel - 1 >= 0 then
                    yield elevatorLevel - 1
                if elevatorLevel + 1 < floors then
                    yield elevatorLevel + 1
            }
        seq {
            let (State(_,elevatorLevel)) = state
            let items = getItems state elevatorLevel |> Seq.toArray
            for newElevatorLevel in getOtherLevels elevatorLevel do
                // move 1 item
                for item in items do
                    let newState = move1Item item elevatorLevel newElevatorLevel state
                    if (isCompatible newState elevatorLevel) && (isCompatible newState newElevatorLevel) then
                        yield newState
                // move 2 items
                for (item1, item2) in take2 items do
                    let newState = move2Items item1 item2 elevatorLevel newElevatorLevel state
                    if (isCompatible newState elevatorLevel) && (isCompatible newState newElevatorLevel) then
                        yield newState
        }

    let bfs() = 
        let q = new List<State>()
        let finalState = State(Array.replicate types (floors-1,floors-1), (floors-1))
        distance.Add(finalState |> getIndex, 0)
        ignore(q.Add(finalState))
        while q.Count > 0 && not found do
            let state = q.[0]
            q.RemoveAt(0)
            let d = distance.[state |> getIndex]
            if d > highestDist then 
                highestDist <- d
                printfn "highest distance = %i" d
            for nb in getNeighbours state do
                edges <- edges + 1
                let nbIndex = nb |> getIndex
                if not(distance.ContainsKey(nbIndex)) then
                    distance.Add(nbIndex, d + 1)
                    q.Add(nb)
                    if nb = initialState then
                        found <- true
    
    let solve() =
        bfs()
        let result = distance.[initialState |> getIndex]
        printfn "%i" result
        printfn "nodes = %i, edges = %i" (distance.Count) edges