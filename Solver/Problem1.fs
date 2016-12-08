module Problem1

    open Util
    open System.Collections.Generic

    type Turn = Left | Right
    type Distance = int
    type Action = Action of Turn * Distance

    type Location = Location of int * int
    type Orientation = N | S | W | E
    type State = State of Location // current location
                          * Orientation // current orientation
                          * Location option // first location seen twice

    let rec makeTurn turn = 
        let makeRightTurn = function 
            | N -> E
            | E -> S
            | S -> W
            | W -> N
        match turn with
        | Right -> makeRightTurn
        | Left -> makeRightTurn >> makeRightTurn >> makeRightTurn

    let updateSeen = 
        let mutable locationsSeen = new HashSet<Location>()

        let inBetweenLocations (Location(x1,y1)) (Location(x2,y2)) = 
            let getseq from too = 
                if from < too then seq { from+1 .. too }
                else seq { from-1 .. -1 .. too }
            if x1 = x2 then
                getseq y1 y2 |> Seq.map (fun y -> Location(x1,y))
            else
                getseq x1 x2 |> Seq.map (fun x -> Location(x,y1))

        (fun (Location(x1,y1)) (Location(x2,y2)) seenTwice ->
            let mutable firstLocationSeenTwice = seenTwice
            for location in inBetweenLocations (Location(x1,y1)) (Location(x2,y2)) do
                if firstLocationSeenTwice = None && locationsSeen.Contains(location) then 
                    firstLocationSeenTwice <- Some(location)
                ignore(locationsSeen.Add(location))
            firstLocationSeenTwice
        )

    let makeSteps (State(Location(x,y), orientation, seenTwice)) (Action(turn, distance)) =
        let orientation = makeTurn turn orientation
        let newLocation = match orientation with
            | N -> Location(x,y+distance)
            | S -> Location(x,y-distance)
            | E -> Location(x+distance,y)
            | W -> Location(x-distance,y)
        let seenTwice = updateSeen (Location(x,y)) newLocation seenTwice
        State(newLocation, orientation, seenTwice)

    let totalDistance (Location(x,y)) = abs(x) + abs(y)

    let parseAction (token: string) =
        let parseTurn = function
            | 'R' -> Right
            | _ -> Left
        Action( parseTurn token.[0], token.Substring(1, token.Length - 1) |> int )

    let parseActions = 
        let sc = Scanner(@"..\..\Problem1.in", " \t\r\n,")
        sc.Tokens |> Seq.map parseAction

    let solve() =
        let initialState = State( Location(0,0), N, None )
        let finalState = parseActions |> Seq.fold makeSteps initialState
        let (State(location, _, seenTwice)) = finalState
        printfn "%d" (totalDistance location)
        let (Location(x,y)) = seenTwice.Value
        printfn "%d %d ==> %d" x y (totalDistance seenTwice.Value)
