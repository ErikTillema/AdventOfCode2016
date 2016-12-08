module Problem5a

    open System.Text
    open System.Security.Cryptography

    let getHash s i =
        let totalString = s + (i |> string)
        use md5 = MD5.Create()
        md5.ComputeHash(Encoding.UTF8.GetBytes(totalString))

    let isGood (hash: byte array) = 
        hash.[0] = 0uy && hash.[1] = 0uy && (hash.[2] / 16uy)= 0uy

    let rec getNextGoodHash s i =
        let hash = getHash s i
        if isGood hash then hash, i
        else getNextGoodHash s (i+1)

    let solve() = 
        let s = "ojvtpuvg" //"abc"
        let mutable i = 0
        for _ in [1..8] do
            let hash, j = getNextGoodHash s i
            let b = hash.[2] % 16uy
            printf "%s" (System.String.Format("{0:X}", b).ToLower())
            i <- j + 1
        printfn ""
        printfn "%i" i
