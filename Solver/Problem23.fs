module Problem23
    
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
        | Tgl of Source

    let state = Dictionary<String,int>()
    let mutable pos = 0
    let mutable instructions: Instruction[] = null

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
        | Some("tgl") -> 
            let target = parseSource (sc.Next().Value)
            Tgl(target)
        | _ -> invalidOp "bad input string"
    
    let parseInstructions() =
        let sc = Scanner(@"..\..\Problem23.test.in")
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

    let toggle instruction = 
        match instruction with
        | Cpy(source, name) -> Jnz(source,name)
        | Inc(name) -> Dec(name)
        | Dec(name) -> Inc(name)
        | Jnz(source, jump) -> Cpy(source,jump)
        | Tgl(name) -> Inc(name)

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
        | Tgl(source) ->
            let value = getValue source
            let targetpos = pos + value
            if 0 <= targetpos && targetpos < instructions.Length then
                let instruction = instructions.[targetpos]
                instructions.[targetpos] <- toggle instruction
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

    let solveSilver() =
        instructions <- parseInstructions()
        followInstructions Int32.MaxValue
        printfn "%i" (getValue (Variable("a")))

    let solveGold() =
        let presetValues n i =
            setValue (Variable("a")) n
            setValue (Variable("b")) i
            setValue (Variable("c")) 0
            setValue (Variable("d")) 0
            pos <- 9

        instructions <- parseInstructions()

        let mutable n = 12
        for i in 11..-1..3 do
            n <- n * i
            presetValues n i
            followInstructions 50 // follow 50 instructions to toggle the right line
        presetValues 479001600 2 // 12!
        followInstructions Int32.MaxValue
        printfn "%i" (getValue (Variable("a")))
