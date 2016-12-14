module Problem4

    open System
    open System.Collections.Generic
    open Util
    
    // sometimes better convert seqs to lists, so that we can iterate them twice
    // better to convert strings to char list instead of char seq, so they are easier to compare, can iterate twice, easier to view in debugger.
    type Room = Room of char list * int * char list 

    let getTop5 s = 
        let dict = new Dictionary<char, int>()
        for c in s do
            if not (dict.ContainsKey(c)) then
                dict.Add(c, 0)
            dict.[c] <- dict.[c] + 1
        dict |> List.ofSeq |> List.map (fun kvp -> (kvp.Key, kvp.Value)) 
             |> List.sortBy (fun (c,count) -> (-count, c)) |> List.take 5 |> List.map fst

    let getTop5v2 (s: char list) = 
        s    |> List.groupBy id |> List.map (fun (key,items) -> (key, List.length items))
             |> List.sortBy (fun (c,count) -> (-count, c)) |> List.take 5 |> List.map fst

    
    let isReal (Room(letters,_,checksum)) =
        let top5 = getTop5 letters
        let top52 = getTop5v2 letters
        let result = top5 = checksum // NB: comparing sequences with = doesn't work!
        result

    let parseRoom (s: String) =
        let i1 = s.LastIndexOf '-'
        let i2 = s.IndexOf '['
        let i3 = s.IndexOf ']'
        let letters = s.Substring(0, i1) |> List.ofSeq |> List.filter ((<>) '-')
        let id = s.Substring(i1+1, i2-i1-1) |> int
        let checksum = s.Substring(i2+1, i3-i2-1) |> List.ofSeq
        Room(letters, id, checksum)

    let decrypt (Room(letters,id,_)) =
        let rotate n c = 
            if c = '-' then 
                ' '
            else
                (((c |> int) - ('a' |> int)) + n) % 26 + ('a' |> int) |> char
        let n = id % 26
        letters |> List.map (rotate n)

    let solve() = 
        let sc = Scanner(@"..\..\Problem4.test.in")
        let rooms = sc.Lines |> Seq.map parseRoom |> List.ofSeq
        let realRooms = rooms |> List.filter isReal
        let sumIds = realRooms |> List.sumBy (fun (Room(_,id,_)) -> id)
        printfn "%i" sumIds
        for room in realRooms do
            let decrypted = decrypt room
            let (Room(_,id,_)) = room
            printfn "%s %i" (decrypted |> List.toArray |> String) id