module Problem19a
    
    open System

    let rec getWinner start distance total =
        match total with 
        | 1 -> start + 1
        | _ ->
            if total % 2 = 1 then
                getWinner (start + 2*distance) (2*distance) ((total-1)/2)
            else
                getWinner (start) (2*distance) (total/2)

    let solve() = 
        let n = 5
        let result = getWinner 0 1 n
        printfn "%i" result



