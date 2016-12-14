module Problem10

    open Util
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions

    type Chip = Chip of int
    type BotId = BotId of int
    type OutputId = OutputId of int 
    type Bot = { id : BotId; mutable chip1 : Chip option; mutable chip2 : Chip option }
    type Output = { id: OutputId; mutable chips: Chip list }
    type Receiver =
        | BotReceiver of Bot
        | OutputReceiver of Output
    type Action = Give of Chip * Bot * Receiver
    type Instruction = Instruction of Bot * Receiver * Receiver

    let mutable result: BotId option = None
    let bots = Dictionary<BotId,Bot>()
    let outputs = Dictionary<OutputId,Output>()
    let instructions = Dictionary<BotId, Instruction>()

    let getBot botId =
        if not (bots.ContainsKey(botId)) then 
            let bot = { id = botId ; chip1 = None ; chip2 = None }
            bots.Add(botId, bot)
        bots.[botId]

    let getOutput outputId =
        if not (outputs.ContainsKey(outputId)) then 
            let output = { id = outputId ; chips = [] }
            outputs.Add(outputId, output)
        outputs.[outputId]

    let giveToBot chip bot = 
        match bot with
        | { id = _ ; chip1 = None ; chip2 = None } -> 
            bot.chip1 <- Some(chip)
        | { id = botId ; chip1 = Some(c) ; chip2 = None } -> 
            let c1 = min chip c
            let c2 = max chip c
            if c1 = Chip(17) && c2 = Chip(61) then  result <- Some(botId)
            //if c1 = Chip(2) && c2 = Chip(5) then  result <- Some(botId)
            bot.chip1 <- Some(c1)
            bot.chip2 <- Some(c2)
        | _ -> invalidOp (String.Format("bot {0} already has two chips", id))

    let giveToOutput chip output = 
        output.chips <- chip::(output.chips)

    let emptyBot bot =
        bot.chip1 <- None
        bot.chip2 <- None 

    let parseLine line = 
        match line with
        | Regex "^value (?<chip>\d+) goes to bot (?<botId>\d+)$" [ chip ; botId ] ->
            let chip = chip |> int |> Chip
            let botId = botId |> int |> BotId
            let bot = getBot botId
            giveToBot chip bot
        | Regex "^bot (?<giverBotId>\d+) gives low to (?<botOrOutputLow>bot|output) (?<idLow>\d+) and high to (?<botOrOutputHigh>bot|output) (?<idHigh>\d+)$" 
                    [ giverBotId ; botOrOutputLow ; idLow ; botOrOutputHigh ; idHigh ] ->
            let getReceiver botOrOutput id = 
                match botOrOutput with
                | "bot" -> 
                    let botId = id |> BotId
                    let bot = getBot botId
                    BotReceiver(bot)
                | _ ->
                    let outputId = id |> OutputId
                    let output = getOutput outputId
                    OutputReceiver(output)
            let giverBotId = giverBotId |> int |> BotId
            let idLow = idLow |> int
            let idHigh = idHigh |> int
            let bot = getBot giverBotId
            let instruction = Instruction(bot, (getReceiver botOrOutputLow idLow), (getReceiver botOrOutputHigh idHigh))
            instructions.Add( giverBotId, instruction)
        | _ -> invalidOp "bad line"

    let parseInput = 
        let sc = Scanner(@"..\..\Problem10.test.in")
        for line in sc.Lines do
            parseLine line

    let getActions() = 
        seq {
            for bot in bots.Values do
                match bot with
                | { id = botId ; chip1 = Some(c1) ; chip2 = Some(c2) } -> 
                        let (Instruction(bot, rec1, rec2)) = instructions.[botId]
                        yield Give(c1, bot, rec1)
                        yield Give(c2, bot, rec2)
                | _ -> ()
        }
        
    let rec doActions() = 
        let doAction (Give(chip,bot,receiver)) = 
            match receiver with
            | BotReceiver(bot) -> 
                giveToBot chip bot
            | OutputReceiver(output) -> 
                giveToOutput chip output
            emptyBot bot

        let mutable actionDone = false
        for action in getActions() do
            doAction action
            actionDone <- true
        if actionDone then doActions()

    let firstOutputs = 
        seq {
            yield! outputs.[OutputId(0)].chips
            yield! outputs.[OutputId(1)].chips
            yield! outputs.[OutputId(2)].chips
        }

    let rec getMultiple n list =
        match list with
        | [] -> n
        | (Chip(x))::xs -> getMultiple (n*x) xs

    let solve() = 
        parseInput
        doActions()
        let (BotId(id)) = result.Value
        printfn "%i" id
        let multiple = getMultiple 1 (firstOutputs |> Seq.toList)
        printfn "%i" multiple