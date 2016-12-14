module Problem2b

    open Util
    open System

    type Position = Position of int * int
    type Move = U | D | L | R
    type Path = Path of Move seq

    let isValid (Position(x,y)) = 
        abs(x) + abs(y) <= 2

    let makeSafeMove position move =
        let makeMove (Position(x,y)) move = 
            match move with
            | U -> Position(x,y+1)
            | D -> Position(x,y-1)
            | R -> Position(x+1,y)
            | L -> Position(x-1,y)
        let newPosition = makeMove position move
        if isValid newPosition then newPosition
        else position

    let makeMoves (Path(moves)) position = 
        moves |> Seq.fold makeSafeMove position

    let number (Position(x,y)) = 
        match y with
        | 2 -> (1 + int '0') |> char
        | 1 -> ((3+x) + int '0') |> char
        | 0 -> ((7+x) + int '0') |> char
        | -1 -> ((1+x) + int 'A') |> char
        | _ -> 'D'

    let parseMove = function
        | 'U' -> U 
        | 'D' -> D
        | 'L' -> L
        | 'R' -> R
        | _ -> invalidOp ""

    let parsePath s = 
        let moves = s |> Seq.map parseMove
        Path(moves)

    let parsePaths = 
        let sc = Scanner(@"..\..\Problem2.test.in")
        sc.Lines |> Seq.map parsePath

    let solve() = 
        let initialState = [], Position(-2,0)
        let folder (chars, position) path = 
            let position = position |> makeMoves path
            let c = number position
            (c::chars, position)
        let (finalChars, _) = parsePaths |> Seq.fold folder initialState
        printfn "%s" (finalChars |> List.rev |> Array.ofList |> String)

