module Problem21
    
    open Util
    open System

    type Operation = 
        | SwapPosition of int * int
        | SwapLetter of char * char
        | RotateLeft of int
        | RotateRight of int
        | RotateByLetter of char
        | Reverse of int * int
        | Move of int * int

    let parseLine (line: String) =
        match line with
        | Regex "^swap position (?<pos1>\d+) with position (?<pos2>\d+)$" [ pos1 ; pos2 ] -> SwapPosition(pos1 |> int, pos2|> int)
        | Regex "^swap letter (?<char1>\w) with letter (?<char2>\w)$" [ char1 ; char2 ] -> SwapLetter(char1|> char, char2|> char)
        | Regex "^rotate left (?<steps>\d+) steps?$" [ steps ] -> RotateLeft(steps |> int)
        | Regex "^rotate right (?<steps>\d+) steps?$" [ steps ] -> RotateRight(steps |> int)
        | Regex "^rotate based on position of letter (?<c>\w)$" [ c ] -> RotateByLetter(c |> char)
        | Regex "^reverse positions (?<pos1>\d+) through (?<pos2>\d+)$" [ pos1 ; pos2 ] -> Reverse(pos1 |> int, pos2|> int)
        | Regex "^move position (?<pos1>\d+) to position (?<pos2>\d+)$" [ pos1 ; pos2 ] -> Move(pos1 |> int, pos2|> int)
        | _ -> invalidOp "bad input"

    let parseLines() =
        let sc = Scanner(@"..\..\Problem21.test.in")
        sc.Lines |> Seq.map parseLine

    let scramble operation (s: char[]) = 
        let n = s.Length
        let rotateRight l =
            let rotated = Array.replicate n ' '
            for i in 0..n-1 do
                rotated.[i] <- s.[(i-l+n+n)%n]
            for i in 0..n-1 do s.[i] <- rotated.[i]
            
        match operation with
        | SwapPosition(p1,p2) ->
            let tmp = s.[p1]
            s.[p1] <- s.[p2]
            s.[p2] <- tmp
        | SwapLetter(c1,c2) ->
            for i in 0..n-1 do
                if s.[i] = c1 then s.[i] <- c2
                elif s.[i] = c2 then s.[i] <- c1
        | RotateLeft(l) ->
            rotateRight (n-l)
        | RotateRight(l) ->
            rotateRight l
        | RotateByLetter(c) ->
            let i = s |> Array.findIndex ((=) c)
            let l = 1 + i + (if i >= 4 then 1 else 0)
            rotateRight l
        | Reverse(from,too) ->
            let nrev = too-from+1
            for i in 0..nrev/2-1 do
                let tmp = s.[from+i]
                s.[from+i] <- s.[from+nrev-1-i]
                s.[from+nrev-1-i] <- tmp
        | Move(from, too) ->
            if from < too then
                let tmp = s.[from]
                for i in from..too-1 do
                    s.[i] <- s.[i+1]
                s.[too] <- tmp
            elif too < from then
                let tmp = s.[from]
                for i in from .. -1 .. too+1 do
                    s.[i] <- s.[i-1]
                s.[too] <- tmp

    let unscramble operation (s: char[]) =
        let n = s.Length
        match operation with
        | SwapPosition(p1,p2) -> scramble operation s
        | SwapLetter(c1,c2) -> scramble operation s
        | RotateLeft(l) -> scramble (RotateRight(l)) s
        | RotateRight(l) -> scramble (RotateLeft(l)) s
        | RotateByLetter(c) -> 
            let np = s |> Array.findIndex ((=) c)
            let p = 
                if np % 2 = 1 then (np-1)/2
                elif np = 0 then   (np-2)/2 + n
                else               (np-2)/2 + n/2
            let l = 1 + p + (if p >= 4 then 1 else 0)
            scramble (RotateLeft(l)) s
        | Reverse(from,too) -> scramble operation s
        | Move(from, too) -> scramble (Move(too,from)) s

    let solveSilver() = 
        let s = "abcde"
        let s = s.ToCharArray()
        let operations = parseLines()
        for operation in operations do
            scramble operation s
        printfn "%s" (s |> String)

    let solveGold() = 
        let s = "fbdecgha"
        let s = s.ToCharArray()
        let operations = parseLines() |> Seq.rev
        for operation in operations do
            unscramble operation s
        printfn "%s" (s |> String)
