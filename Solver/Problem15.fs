module Problem15

    open MathExt

    //let discs = [| (1,5,4); (2,2,1); |] // (index, slots, position)
    let discs = [| (1,5,2); (2,13,7);  (3,17,10);  (4,3,2); (5,19,9); (6,7,0); (7,11,0); |] // (index, slots, position)

    // p prime
    // solves a*x = b (mod p)
    let solveModulo p a b = 
        let inv = inverseModulo p a // a * inv = 1 (mod p)
        b * inv |> modulo p

    let solve() = 
        let mutable result = 0
        let mutable n = 1
        for (index,slots,position) in discs do
            let x = solveModulo slots n (-(index+position+result)) 
            result <- result + x * n
            n <- n * slots

        printfn "%s" (result.ToString())
