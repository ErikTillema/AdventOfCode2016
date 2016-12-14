module Problem9b

    open System
    open System.Text.RegularExpressions
    open Util

    let rec getLength s =
        match s with
        | Regex "^(?<head>.*?)\((?<chars>\d+)x(?<repeat>\d+)\)(?<tail>.*)$" [ head; chars; repeat; tail ] ->
            let chars = chars |> int
            let repeat = repeat |> int
            if tail.Length < chars then invalidOp "not enough characters in tail"
            let repeatPart = tail.Substring(0, chars)
            let tail = tail.Substring(chars)
            (head.Length |> int64) + (repeat |> int64) * (getLength repeatPart) + (getLength tail)
        | _ -> s.Length |> int64

    let solve() =
        let sc = Scanner(@"..\..\Problem9.test.in")
        let lengths = sc.Lines |> Seq.map getLength
        for l in lengths do
            printfn "%i" l
