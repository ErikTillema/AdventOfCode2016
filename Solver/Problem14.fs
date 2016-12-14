module Problem14

    open System
    open System.Collections.Generic
    open System.Text
    open System.Security.Cryptography

    let hashes = 2017 // 1 for silver, 2017 for gold
    let prefix = "abc" //"abc" // "yjdafjpo"
    let mutable lastCalculated = -1
    let triplets = List<int*byte>() // mutable
    let lastQuintet = Dictionary<byte,int>() // mutable

    let expandBytes bytes = 
        seq {
            for b in bytes do
                yield b / 16uy
                yield b % 16uy
        } |> Seq.toArray

    let getHash i = 
        let toCharByte (b: byte) = 
            if b < 10uy then b + ('0' |> byte)
            else             b - 10uy + ('a' |> byte)
        use md5 = MD5.Create()
        let totalString = prefix + (i |> string)
        let mutable bytes = Encoding.UTF8.GetBytes(totalString)
        for i in 1..hashes do
            if i > 1 then bytes <- bytes |> Array.map toCharByte
            bytes <- md5.ComputeHash(bytes)
            bytes <- expandBytes bytes
        bytes

    let getFirstTriplet (hash: byte[]) =
        let mutable l = 1
        let mutable result = None
        for i in 1..hash.Length-1 do
            if hash.[i] = hash.[i-1] then
                l <- l + 1
                if l = 3 && result = None then 
                    result <- Some(hash.[i])
            else
                l <- 1
        result

    let getAllQuintets (hash: byte[]) =
        let mutable l = 1
        seq {
            for i in 1..hash.Length-1 do
                if hash.[i] = hash.[i-1] then
                    l <- l + 1
                    if l = 5 then 
                        yield hash.[i]
                else
                    l <- 1
        } |> Seq.distinct
    
    let updateLastQuintet b i = 
        if not (lastQuintet.ContainsKey(b)) then lastQuintet.Add(b, 0)
        lastQuintet.[b] <- i

    let updateState() =
        lastCalculated <- lastCalculated + 1
        let hash = getHash lastCalculated
        match getFirstTriplet hash with
        | Some(b) -> 
            triplets.Add(lastCalculated,b)
            for b in getAllQuintets hash do
                updateLastQuintet b lastCalculated
        | None -> ()

    let updateStates until = 
        while lastCalculated < until do
            updateState()

    let solve() =
        let mutable keys = 0
        let mutable tripletIndex = 0
        while keys < 64 do
            while tripletIndex >= triplets.Count do
                updateState() // find next triplet

            let (j,b) = triplets.[tripletIndex]
            tripletIndex <- tripletIndex + 1

            // update state
            updateStates (j+1000)
            let isKey = lastQuintet.ContainsKey(b) && lastQuintet.[b] > j

            if isKey then 
                printfn "%i" j
                keys <- keys + 1
        