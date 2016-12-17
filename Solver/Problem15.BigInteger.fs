module Problem15_BigInteger

    open BigIntegerExt
    open System.Numerics

    //let discs = [| (1,5,4); (2,2,1); |] // (index, slots, position)
    //let discs = [| (1,5,2); (2,13,7);  (3,17,10);  (4,3,2); (5,19,9); (6,7,0); (7,11,0); |] // (index, slots, position)
    // let's increase the primes to make it more interesting!
    //let discs = [| (1,1009,2); (2,1013,7);  (3,1019,10);  (4,1021,2); (5,1031,9); (6,1033,0); (7,1039,0); |] // (index, slots, position)
    // let's increase the primes even more!
    let discs = [| (1,100000007,2); (2,100000037,7);  (3,100000039,10);  (4,100000049,2); (5,100000073,9); (6,100000081,0); (7,100000123,0); |]

    // p prime
    // solves a*x = b (mod p)
    let solveModulo p a b =
        let b = modulo p b
        let inv = inverseModulo p a // a * inv = 1 (mod p)
        let x = b * inv
        let x = modulo p x
        x

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
