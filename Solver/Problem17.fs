module Problem17
    
    open System
    open System.Collections.Generic
    open System.Text
    open System.Security.Cryptography

    let w, h = 4, 4
    let input = "vwbaicqe" // "ulqzkmiv" // "ihgpwlah" // "hijkl" // "vwbaicqe"
    let md5 = MD5.Create()

    let dx = [| 0; 0; -1; 1 |]
    let dy = [| 1; -1; 0; 0 |]

    let getChar i = 
        match i with
        | 0 -> "U"
        | 1 -> "D"
        | 2 -> "L"
        | 3 -> "R"
        | _ -> invalidOp "bad direction"

    let isOpen (bytes: byte[]) i =
        match i with
        | 0 -> bytes.[0] / 16uy > 10uy // up
        | 1 -> bytes.[0] % 16uy > 10uy // down
        | 2 -> bytes.[1] / 16uy > 10uy // left
        | 3 -> bytes.[1] % 16uy > 10uy // right
        | _ -> invalidOp "bad direction"

    let bfsSilver start endd =
        let q = List<int*int*string>()
        let startx,starty = start
        let endx,endy = endd
        q.Add(startx,starty,input)
        let mutable result = None
        while q.Count > 0 && result.IsNone do
            let x,y,s = q.[0]
            q.RemoveAt(0)
            if x = endx && y = endy then
                result <- Some(s)
            let bytes = md5.ComputeHash(Encoding.UTF8.GetBytes(s))
            for i in 0..3 do
                let nx, ny = x+dx.[i], y+dy.[i]
                if 0 <= nx && nx < w && 0 <= ny && ny < h then
                    if isOpen bytes i then
                        q.Add(nx,ny,s+getChar(i))
        result
        
    let bfsGold start endd =
        let q = List<int*int*string*int>()
        let startx,starty = start
        let endx,endy = endd
        q.Add(startx,starty,input,0)
        let mutable result = None
        while q.Count > 0 do
            let x,y,s,d = q.[0]
            q.RemoveAt(0)
            if x = endx && y = endy then
                match result with 
                | Some(dd) -> result <- Some(max d dd)
                | None -> result <- Some(d)
            else 
                let bytes = md5.ComputeHash(Encoding.UTF8.GetBytes(s))
                for i in 0..3 do
                    let nx, ny = x+dx.[i], y+dy.[i]
                    if 0 <= nx && nx < w && 0 <= ny && ny < h then
                        if isOpen bytes i then
                            q.Add(nx,ny,s+getChar(i),d+1)
        result

    let solve() = 
        let start = (0,3)
        let endd = (3,0)
        let path = bfsSilver start endd
        printfn "%s" (path.Value.Substring(input.Length))
        let longest = bfsGold start endd
        printfn "%i" (longest.Value)

