module Problem7b

    open Util
    open System

    let isAba i (s: char array) =
        s.[i] = s.[i+2] && s.[i] <> s.[i+1]

    let getAbas (s: char array) = 
        let getAba i s = 
            if isAba i s then Some(s.[i], s.[i+1]) 
            else None
            
        [0..(s.Length-3)] |> List.map (fun i -> getAba i s) |> List.filter Option.isSome |> List.map Option.get

    let reverseAba (c1,c2) = (c2,c1)

    let isSsl (parts1: char array list) (parts2: char array list) =
        let abas1 = parts1 |> List.collect getAbas
        let abas2 = parts2 |> List.collect getAbas |> List.map reverseAba |> Set.ofList
        let result = abas1 |> List.exists (fun aba -> abas2.Contains(aba))
        if result then printfn "%s" (parts1 |> List.map String |> String.concat " ")
        result

    let parseParts (line: String) = 
        let sc = Scanner(line, "[]", false)
        let parts1, parts2 = sc.Tokens |> Seq.mapi (fun i token -> (i%2=0, token)) |> Seq.toList |> List.partition fst
        let parts1 = parts1 |> List.map snd |> List.map (fun s -> s.ToCharArray())
        let parts2 = parts2 |> List.map snd |> List.map (fun s -> s.ToCharArray())
        parts1, parts2

    let solve() =
        let sc = Scanner(@"..\..\Problem7.in") // @"..\..\Problem7b.test.in"
        let n = sc.Lines |> Seq.map parseParts |> Seq.filter (fun (p1,p2) -> isSsl p1 p2) |> Seq.length
        printfn "%i" n