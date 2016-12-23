module Problem19b_CircularList
    
    open System.Collections.Generic

    type CircleList<'a> = { Element: 'a ; mutable Tail: CircleList<'a> option }

    let solveCircleList (list: CircleList<int>) n (remove: bool[]) i = 
        let mutable i = i
        let mutable n = n
        let mutable cur = list
        while n > 1 do
            if remove.[i] then
                //remove next
                cur.Tail <- Some(cur.Tail.Value.Tail.Value)
                n <- n-1
            else
                //skip
                cur <- cur.Tail.Value
            i <- (i + 1)%3
        cur.Element
                   
    let solve() =
        let n = 5
        let endd = { Element = (n-1); Tail = None }
        let mutable head = endd
        for i in n-2..-1..0 do 
            head <- {Element = i; Tail = Some(head) }
        endd.Tail <- Some(head)
        let remove = Array.replicate 3 true
        if n % 2 = 1 then remove.[1] <- false
        else remove.[2] <- false
        let mutable pos = head
        for i in 0..n/2-2 do pos <- pos.Tail.Value
        let result = solveCircleList pos n remove 0
        printfn "%i" (result+1)
