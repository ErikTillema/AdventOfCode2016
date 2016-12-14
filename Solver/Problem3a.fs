module Problem3a

    open Util

    let isValidTriangle sides = 
        let sortedSides = Array.sort sides
        sortedSides.[0] + sortedSides.[1] > sortedSides.[2]

    let rec parseTriangles = 
        let sc = Scanner(@"..\..\Problem3.test.in")
        seq {
            match sc.NextInt(), sc.NextInt(), sc.NextInt() with
            | Some(s1), Some(s2), Some(s3) -> 
                yield [| s1; s2; s3 |]
                yield! parseTriangles
            | _ -> ()
        }

    let solve() = 
        let triangles = parseTriangles
        let validCount = triangles |> Seq.filter isValidTriangle |> Seq.length
        printfn "%i" validCount
