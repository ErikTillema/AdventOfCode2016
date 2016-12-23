module Problem19b_LinkedList
    
    open System.Collections.Generic

    type NonEmptyRecord<'a> = { Element: 'a ; mutable Tail: LL<'a> }
    and  LL<'a> = 
        | Empty
        | NonEmpty of NonEmptyRecord<'a>

    let solveLinkedList (list: LL<int>) n (remove: bool[]) i = 
        let mutable i = i
        let mutable n = n
        let mutable cur = list
        while n > 1 do
            match cur with
            | Empty -> invalidOp "kan niet 1"
            | NonEmpty(record) ->
                if remove.[i] then
                    //remove next
                    match record.Tail with
                    | Empty -> invalidOp "kan niet 2"
                    | NonEmpty({ Element=_; Tail=tailtail }) ->
                        record.Tail <- tailtail
                        n <- n-1
                else
                    //skip
                    cur <- record.Tail
                i <- (i + 1)%3
        
        match cur with
        | Empty -> invalidOp "mag niet 3"
        | NonEmpty({ Element=result; Tail=_ }) -> 
            result

    let solve() =
        let n = 5
        let endd = { Element = (n-1); Tail = LL.Empty }
        let mutable list = NonEmpty (endd)
        for i in n-2..-1..0 do 
            list <- LL.NonEmpty({Element = i; Tail = list })
        let head = list
        endd.Tail <- head
        let remove = Array.replicate 3 true
        if n % 2 = 1 then remove.[1] <- false
        else remove.[2] <- false
        let mutable pos = head
        for i in 0..n/2-2 do 
            let (NonEmpty({Element=_; Tail=tail})) = pos
            pos <- tail
        let result = solveLinkedList pos n remove 0
        printfn "%i" (result+1)
