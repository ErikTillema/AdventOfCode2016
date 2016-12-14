module Problem3b

    open Util

    let isValidTriangle sides = 
        let sortedSides = Array.sort sides
        sortedSides.[0] + sortedSides.[1] > sortedSides.[2]

    let rec parseTriangles = 
        let sc = Scanner(@"..\..\Problem3.test.in")
        seq {
            match sc.NextInt(), sc.NextInt(), sc.NextInt(), 
                  sc.NextInt(), sc.NextInt(), sc.NextInt(), 
                  sc.NextInt(), sc.NextInt(), sc.NextInt()  with
            | Some(a1), Some(a2), Some(a3), Some(b1), Some(b2), Some(b3), Some(c1), Some(c2), Some(c3) -> 
                yield [| a1; b1; c1 |]
                yield [| a2; b2; c2 |]
                yield [| a3; b3; c3 |]
                yield! parseTriangles
            | _ -> ()
        }

    let solve() = 
        let triangles = parseTriangles
        let validCount = triangles |> Seq.filter isValidTriangle |> Seq.length
        printfn "%i" validCount
