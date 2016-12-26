module Problem25
    
    open Util
    open System
    open System.Collections.Generic
    open System.Numerics

    type Source = 
        | Variable of String
        | Value of int

    type Instruction = 
        | Cpy of Source * Source
        | Inc of Source
        | Dec of Source
        | Jnz of Source * Source
        | Out of Source

    let state = Dictionary<String,int>()
    let mutable pos = 0
    let mutable instructions: Instruction[] = null
    let mutable output = []

    let parseInstruction line = 
        let parseSource s = 
            try
                let i = s |> int
                Value(i)
            with
            | _ -> Variable(s)
        let sc = Scanner(line, " ", false)
        match sc.Next() with
        | Some("cpy") ->
            let source = parseSource (sc.Next().Value)
            let target = parseSource (sc.Next().Value)
            Cpy(source, target)
        | Some("inc") -> 
            let target = parseSource (sc.Next().Value)
            Inc(target)
        | Some("dec") -> 
            let target = parseSource (sc.Next().Value)
            Dec(target)
        | Some("jnz") -> 
            let source = parseSource (sc.Next().Value)
            let i = parseSource (sc.Next().Value)
            Jnz(source, i)
        | Some("out") -> 
            let source = parseSource (sc.Next().Value)
            Out(source)
        | _ -> invalidOp "bad input string"
    
    let parseInstructions() =
        let sc = Scanner(@"..\..\Problem25.in")
        sc.Lines |> Seq.map parseInstruction |> Seq.toArray

    let init source = 
        match source with
        | Value(_) -> ()
        | Variable(name) -> if not (state.ContainsKey(name)) then state.Add(name, 0)

    let getValue source = 
        init source
        match source with
        | Value(i) -> i
        | Variable(name) -> state.[name]

    let setValue source i = 
        init source
        match source with
        | Value(_) -> ()
        | Variable(name) -> state.[name] <- i

    let followInstruction() = 
        let instruction = instructions.[pos]
        match instruction with
        | Cpy(source, target) -> 
            setValue target (getValue source)
            pos <- pos + 1
        | Inc(source) -> 
            setValue source (getValue source + 1)
            pos <- pos + 1
        | Dec(source) -> 
            setValue source (getValue source - 1)
            pos <- pos + 1
        | Jnz(source, jump) -> 
            let value = getValue source
            if value <> 0 then
                pos <- pos + (getValue jump)
            else
                pos <- pos + 1
        | Out(source) -> 
            let value = getValue source
            output <- value :: output
            pos <- pos + 1
    
    let printState() =
        printfn "%3i %3i %3i %3i - %3i" (getValue (Variable("a"))) (getValue (Variable("b"))) (getValue (Variable("c"))) (getValue (Variable("d"))) pos

    let rec followInstructions max = 
        if max = 0 then ()
        else 
            if pos >= instructions.Length then ()
            else 
                followInstruction()
                //printState()
                followInstructions (max-1)

    let isOk output = 
        let isOk' output = output |> List.mapi (fun i o -> (i%2,o)) |> List.forall (fun (i,o) -> i=o)
        (List.length output >= 2) && (isOk' output)

    let solve() =
        instructions <- parseInstructions()
        let mutable found = false
        let mutable j = 0
        while not found do
            pos <- 0
            output <- []
            setValue (Variable("a")) j
            setValue (Variable("b")) 0
            setValue (Variable("c")) 0
            setValue (Variable("d")) 0
            followInstructions 100000 //Int32.MaxValue
            output <- List.rev output

            printf "output for a = %10i :" j
            output |> List.iter (fun i -> printf "%i " i)
            printfn ""

            if isOk output then
                found <- true

            j <- j + 1

