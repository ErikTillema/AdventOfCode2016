module Problem5b

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

    let updatePassword pos digit (password: byte option array) = 
        if pos > 7uy then ()
        else
            let pos = pos |> int
            match password.[pos] with
            | None -> 
                password.[pos] <- Some(digit)
                printfn "%i" pos
                ()
            | Some(_) -> ()

    let isFinished password = 
        password |> Array.forall Option.isSome

    let rec getPassword s i password = 
        if isFinished password then ()
        else
            let hash, j = getNextGoodHash s i
            let pos = hash.[2] % 16uy
            let digit = hash.[3] / 16uy
            updatePassword pos digit password
            getPassword s (j+1) password

    let solve() = 
        let s = "abc"
        let password = (Array.create 8 None)
        getPassword s 0 password
        for b in password do
            printf "%s" (System.String.Format("{0:X}", b.Value).ToLower())
