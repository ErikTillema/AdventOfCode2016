module Problem2a

    open Util

    type Position = Position of int * int
    type Move = U | D | L | R
    type Path = Path of Move seq

    let truncate (Position(x,y)) = 
        let truncate' a = min 2 (max 0 a)
        Position( (truncate' x) , (truncate' y) )

    let makeMove (Position(x,y)) move = 
        match move with
        | U -> Position(x,y+1) |> truncate
        | D -> Position(x,y-1) |> truncate
        | R -> Position(x+1,y) |> truncate
        | L -> Position(x-1,y) |> truncate

    let makeMoves (Path(moves)) position = 
        moves |> Seq.fold makeMove position

    let number (Position(x,y)) = 
        7 + x - (3 * y)

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
        let sc = Scanner(@"..\..\Problem2.in")
        sc.Lines |> Seq.map parsePath

    let solve() = 
        let mutable position = Position(1,1)
        for path in parsePaths do
            position <- position |> makeMoves path
            let n = number position
            printf "%i" n
