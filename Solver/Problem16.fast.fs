module Problem16

    open System
    open System.Collections.Generic

    let getBitField (s: String) = 
        [ for c in s -> c = '1' ] |> List

    let n = 35651584 // 20 // 272 // 35651584
    let bitField = getBitField("10111100110001111") //"10000" // "10111100110001111" // mutable

    let double() =
        let n = bitField.Count
        bitField.Add(false)
        for i in 0..n-1 do
            bitField.Add(not(bitField.[n-1-i]))

    let doubleUntil n =
        while bitField.Count < n do double()
        bitField.RemoveRange(n, bitField.Count - n)

    let getChecksum() = 
        let getChecksum' a b = a=b
        while bitField.Count % 2 = 0 do
            let l = bitField.Count
            for i in 0..l/2-1 do
                bitField.[i] <- getChecksum' bitField.[2*i] bitField.[2*i+1]
            bitField.RemoveRange(l/2, l/2)

    let solve() = 
        doubleUntil n
        getChecksum()
        printfn "%s" (bitField |> Seq.map (fun b -> if b then "1" else "0") |> String.concat "")
        ()