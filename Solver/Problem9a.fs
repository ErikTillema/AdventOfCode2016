module Problem9a

    open System
    open System.Text.RegularExpressions
    open Util

    let decompress s =
        let rec decompress' acc rest =
            let decompressOne s =
                match s with 
                | Regex "^(?<head>.*?)\((?<chars>\d+)x(?<repeat>\d+)\)(?<tail>.*)$" [head ; chars ; repeat ; tail] -> 
                    let chars = chars |> int
                    let repeat = repeat |> int
                    if tail.Length < chars then invalidOp "not enough characters in tail"
                    let repeatPart = tail.Substring(0, chars)
                    let tail = tail.Substring(chars)
                    let decompressedSeq = seq {
                        yield head
                        for _ in 1..repeat do
                            yield repeatPart
                    }
                    (decompressedSeq, tail)
                | _ -> ( [s] :> seq<_> , "")

            match rest with
            | "" -> acc
            | _ ->
                match decompressOne rest with 
                | decompressed, "" -> 
                    Seq.append acc decompressed
                | decompressed, tail -> 
                    decompress' (Seq.append acc decompressed) tail

        let result = decompress' [] s
        result |> String.concat ""

    let solve() =
        let sc = Scanner(@"..\..\Problem9a.test.in")
        let decompressed = sc.Lines |> Seq.map decompress
        for s in decompressed do
            printfn "%s %i" s (s.Length)
