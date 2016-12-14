module Problem7a

    open Util
    open System

    let isAbba (s: char array) = 
        let isAbba' i =
            s.[i] = s.[i+3] && s.[i+1] = s.[i+2] && s.[i] <> s.[i+1]
        [0..(s.Length - 4)] |> List.map (fun i -> isAbba' i) |> List.exists (id)

    let isTls (parts1: char array list) (parts2: char array list) =
        let res1 = parts1 |> Seq.exists isAbba
        let res2 = parts2 |> Seq.forall (isAbba >> not)
        let result = res1 && res2
        if result then printfn "yes %s" (parts1 |> Seq.head |> String)
        result

    let parseTls (line: String) = 
        let sc = Scanner(line, "[]", false)
        let parts1, parts2 = sc.Tokens |> Seq.mapi (fun i token -> (i%2=0, token)) |> Seq.toList |> List.partition fst
        let parts1 = parts1 |> List.map snd |> List.map (fun s -> s.ToCharArray())
        let parts2 = parts2 |> List.map snd |> List.map (fun s -> s.ToCharArray())
        parts1, parts2

    let rec parseTlss = 
        let sc = Scanner(@"..\..\Problem7.test.in");
        sc.Lines |> Seq.map parseTls

    let solve() =
        let n = parseTlss |> Seq.filter (fun (p1, p2) -> isTls p1 p2) |> Seq.length
        printfn "%i" n
        
                