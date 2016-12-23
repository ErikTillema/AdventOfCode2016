module Problem19b
    
    open System.Collections.Generic

    // if n%3 <> 0, remove opposite one of start and continue.
    // if n%3 = 0, take one third and set start+2 as new starting point.
    // operations = c * n * (avg[0,1,2] + 1/3) * (1 + 1/3 + 1/9 + ...) = c * n * 4/3 * 3/2 = c * n * 2
    let rec getWinner start (arr: int[]) =
        let n = arr.Length
        if n = 1 then
            arr.[0]
        else
            if n%3 = 0 then
                // remove two thirds of circle, reset starting point.
                let newArr = [| for i in 0..n/3-1 do yield arr.[(start+3*i+2)%n] |]
                let newStart = 0
                getWinner newStart newArr
            else
                // remove 1 at opposite side
                let opp = (start + (n/2))%n
                let newArr = [| for i in 0..n-1 do if i <> opp then yield arr.[i] |]
                let newStart = (start+1)%(n-1)
                getWinner newStart newArr
    
    let solve() =
        let n = 5
        let arr = Array.init n (fun i -> i)
        let result = getWinner 0 arr
        printfn "%i" (result+1)

