module Problem15

    open MathExt

    let discs = [| (1,5,4); (2,2,1); |] // (index, slots, position)

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
