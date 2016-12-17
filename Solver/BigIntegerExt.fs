module BigIntegerExt

    open System.Numerics

    let two = BigInteger(2)

    let modulo (p: BigInteger) (a: BigInteger) = ((a%p)+p)%p

    let inverseModulo p a =
        BigInteger.ModPow(a, p-two, p) |> modulo p

