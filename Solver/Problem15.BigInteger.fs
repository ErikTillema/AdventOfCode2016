module Problem15_BigInteger

    open BigIntegerExt
    open System.Numerics

    //let discs = [| (1,5,4); (2,2,1); |] // (index, slots, position)
    // let's increase the primes to make it more interesting!
    let discs = [| (1,100000007,1); (2,100000037,2);  (3,100000039,3);  (4,100000049,4); (5,100000073,5); (6,100000081,6); (7,100000123,7); |]

    // p prime
    // solves a*x = b (mod p)
    let solveModulo p a b =
        let inv = inverseModulo p a // a * inv = 1 (mod p)
        b * inv |> modulo p

    let solve() = 
        let mutable result = BigInteger.Zero
        let mutable n = BigInteger.One
        for (index,slots,position) in discs do
            let index = BigInteger(index)
            let position = BigInteger(position)
            let slots = BigInteger(slots)
            let x = solveModulo slots n (-(index+position+result)) 
            result <- result + x * n
            n <- n * slots

        printfn "%s" (result.ToString())
