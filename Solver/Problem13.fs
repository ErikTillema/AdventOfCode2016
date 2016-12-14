module Problem13

    open Util
    open System
    open System.Collections.Generic

    let a = 1362 // 10 // 1362
    let distance = Dictionary<int*int,int>() // mutable
    let dx = [| -1; 1; 0; 0 |]
    let dy = [| 0; 0; -1; 1 |]

    let isOpenSpace =
        let isOpenSpace' x y =
            if x < 0 || y < 0 then false
            else 
                let bitCount n = 
                    let rec bitCount' acc n = 
                        if n = 0 then acc
                        else 
                            bitCount' (acc + (n%2)) (n/2)
                    bitCount' 0 n
                let n = x*x + 3*x + 2*x*y + y + y*y + a
                let bc = bitCount n
                (bc % 2) = 0
        let cache = Dictionary<int*int, bool>()
        (fun x y ->
            if not(cache.ContainsKey(x,y)) then
                let result = isOpenSpace' x y
                cache.Add((x,y), result)
            cache.[(x,y)]
        )

    let bfs from too = 
        let mutable tooFound = false
        let q = List<int*int>()
        distance.Clear()
        distance.Add(from, 0)
        q.Add(from)
        while q.Count > 0 && not tooFound do
            let x,y = q.[0]
            q.RemoveAt(0)
            let d = distance.[(x,y)]
            for i in 0..3 do
                let nx , ny = x+dx.[i] , y+dy.[i]
                if isOpenSpace nx ny then
                    let newPos = (nx,ny)
                    if not (distance.ContainsKey(newPos)) then
                        distance.Add(newPos, d+1)
                        q.Add(newPos)
                        if newPos = too then tooFound <- true

    let bfs2 from maxSteps = 
        let q = List<int*int>()
        distance.Clear()
        distance.Add(from, 0)
        q.Add(from)
        while q.Count > 0 do
            let x,y = q.[0]
            q.RemoveAt(0)
            let d = distance.[(x,y)]
            for i in 0..3 do
                let nx , ny = x+dx.[i] , y+dy.[i]
                if isOpenSpace nx ny then
                    let newPos = (nx,ny)
                    if not (distance.ContainsKey(newPos)) then
                        if d+1 <= maxSteps then distance.Add(newPos, d+1)
                        if d+1 < maxSteps  then q.Add(newPos)

    let solve() =
        let from = (1,1)
        let too = (31,39) // (7,4)
        bfs from too
        let result = distance.[too]
        printfn "%i" result
        bfs2 from 50
        let locations = distance.Count
        printfn "%i" locations
