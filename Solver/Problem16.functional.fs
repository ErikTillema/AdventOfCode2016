module Problem16_Functional

    open System
    open System.Collections.Generic

    let getBitField (s: String) = 
        [ for c in s -> c = '1' ]

    let n = 20 // 20 // 272 // 35651584
    let start = getBitField("10111100110001111") //"10000" // "10111100110001111"

    let flip bitField = 
        bitField |> List.map not

    let double bitField =
        let b = bitField |> List.rev |> flip
        let tail = false :: b
        bitField @ tail

    let rec doubleUntil n bitField =
        let l = bitField |> List.length
        if l < n then doubleUntil n (double bitField)
        else bitField |> List.take n

    let rec getChecksum bitField = 
        let getChecksum' a b = a=b
        let l = bitField |> Array.length
        if l % 2 = 1 then bitField
        else 
            let result = [| for i in 0..l/2-1 -> getChecksum' bitField.[2*i] bitField.[2*i+1] |]
            getChecksum result

    let solve() = 
        let bitField = doubleUntil n start
        let checksum = bitField |> Array.ofSeq |> getChecksum
        printfn "%s" (bitField |> Seq.map (fun b -> if b then "1" else "0") |> String.concat "")
        ()