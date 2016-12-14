module Problem8

    open Util
    open System

    type Instruction = 
        | Rect of int * int
        | RotateRow of int * int
        | RotateColumn of int * int

    let updateGrid instruction (grid: bool[,]) = 
        let w = grid.GetLength(0)
        let h = grid.GetLength(1)
        match instruction with
        | Rect(w,h) ->
            for x in 0..w-1 do
                for y in 0..h-1 do
                    grid.[x,y] <- true
        | RotateRow(y, d) -> 
            let temp = Array.init w (fun _ -> false)
            for x in 0..w-1 do
                temp.[(x+d)%w] <- grid.[x,y]
            for x in 0..w-1 do
                grid.[x,y] <- temp.[x]
        | RotateColumn(x, d) ->
            let temp = Array.init h (fun _ -> false)
            for y in 0..h-1 do
                temp.[(y+d)%h] <- grid.[x,y]
            for y in 0..h-1 do
                grid.[x,y] <- temp.[y]

    let countLit (grid: bool[,]) =
        grid |> Seq.cast<_> // unfortunately we need a cast to convert the Array2D to a typed sequence. I don't completely understand...
             |> Seq.filter id |> Seq.length

    let parseInstruction (line: String) = 
        let sc = Scanner(line, " \txy=", false)
        match sc.Next() with
        | Some("rect") -> 
            let w = sc.NextInt().Value
            let h = sc.NextInt().Value
            Rect(w,h)
        | _ -> 
            match sc.Next() with
            | Some("column") ->
                let x = sc.NextInt().Value
                ignore(sc.Next())
                let d = sc.NextInt().Value
                RotateColumn(x,d)
            | _ ->
                let y = sc.NextInt().Value
                ignore(sc.Next())
                let d = sc.NextInt().Value
                RotateRow(y,d)

    let parseInstructions =
        let sc = Scanner(@"..\..\Problem8.test.in")
        sc.Lines |> Seq.map parseInstruction

    let printGrid (grid: bool[,]) = 
        let w = grid.GetLength(0)
        let h = grid.GetLength(1)
        for y in 0..h-1 do
            for x in 0..w-1 do
                printf "%s" (if grid.[x,y] then "#" else ".")
            printfn ""

    let solve() =
        let grid = Array2D.init 7 3 (fun _ _ -> false)
        for ins in parseInstructions do
            updateGrid ins grid
        printGrid grid
        let n = grid |> countLit 
        printfn "%i" n
