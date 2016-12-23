module Problem19b_Faster
    
    open System.Collections.Generic
    
    // if n%3 <> 0, remember which 1 or 2 opposites to remove
    // then take one third in same way but carefully skip the removed opposites.
    // iterations = log3 n
    // for each iteration 1/3 of array is read.
    // so total operations = c * n * (1/3 + 1/9 + ... ) = c * n/2
    let rec getWinner start (arr: int[]) =
        let n = arr.Length
        if n = 1 then
            arr.[0]
        elif n = 2 then
            arr.[start]
        else
            let mutable n = n
            let mutable start = start
            let deletes = List<int>()
            while n%3 > 0 do
                // remove 1 at opposite side
                let opp = (start + (n/2))%n
                deletes.Add(opp)
                n <- n-1
                start <- (start+1)%n

            // remove two thirds of circle, reset starting point.
            let mutable skip = 0
            let newArr = [| 
                            let mutable pos = (start + 2)%n
                            for i in 0..n/3-1 do 
                                while skip < deletes.Count && deletes.[skip] <= pos do 
                                    skip <- skip + 1
                                    pos <- pos + 1
                                yield arr.[pos] 
                                pos <- (pos + 3)%n
                         |]
            let newStart = 0
            getWinner newStart newArr

    let solve() =
        let n = 5
        let arr = Array.init n (fun i -> i)
        let result = getWinner 0 arr
        printfn "%i" (result+1)
