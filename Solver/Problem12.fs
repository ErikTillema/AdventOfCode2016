module Problem12

    open Util
    open System
    open System.Collections.Generic

    type Source = 
        | Variable of String
        | Value of int
    type Instruction = 
        | Cpy of Source * String
        | Inc of String
        | Dec of String
        | Jnz of Source * int

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
            let target = sc.Next().Value
            Cpy(source, target)
        | Some("inc") -> 
            let target = sc.Next().Value
            Inc(target)
        | Some("dec") -> 
            let target = sc.Next().Value
            Dec(target)
        | Some("jnz") -> 
            let source = parseSource (sc.Next().Value)
            let i = sc.NextInt().Value
            Jnz(source, i)
        | _ -> invalidOp "bad input string"
    
    let parseInstructions() =
        let sc = Scanner(@"..\..\Problem12.test.in")
        sc.Lines |> Seq.map parseInstruction |> Seq.toArray

    let init name = 
        if not (state.ContainsKey(name)) then state.Add(name, 0)

    let getValue source = 
        match source with
        | Value(i) -> i
        | Variable(name) -> 
            init name
            state.[name]

    let setValue name i = 
        init name
        state.[name] <- i

    let followInstruction() = 
        let instruction = instructions.[pos]
        match instruction with
        | Cpy(source, name) -> 
            let value = getValue source
            setValue name value
            pos <- pos + 1
        | Inc(name) -> 
            init name
            state.[name] <- state.[name] + 1
            pos <- pos + 1
        | Dec(name) -> 
            init name
            state.[name] <- state.[name] - 1
            pos <- pos + 1
        | Jnz(source, jump) -> 
            let value = getValue source
            if value <> 0 then
                pos <- pos + jump
            else
                pos <- pos + 1

    let rec followInstructions() = 
        if pos >= instructions.Length then ()
        else 
            followInstruction()
            followInstructions()

    let solve() =
        instructions <- parseInstructions()
        setValue "c" 1 // only for gold star.
        followInstructions()
        printfn "%i" (getValue (Variable("a")))
        ()