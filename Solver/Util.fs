module Util

open System
open System.IO
open System.Linq

type Scanner(filepath: string, ?separators0: string, ?isFile0: bool) = 

    let isFile = defaultArg isFile0 true
    let separators = defaultArg separators0 " \t\r\n"
    let mutable atEndOfFile = false
    let reader = 
        if isFile then new StreamReader(filepath) :> TextReader // @@@ using statement?
        else new StringReader(filepath) :> TextReader

    let rec nextTokenOrEOF() = 
        let i = reader.Read()
        if i = -1 then
            atEndOfFile <- true
            ""
        else
            let c = char i
            if separators.Contains(c) then ""
            else (string c + nextTokenOrEOF())
    
    let rec next() = 
        if atEndOfFile then None
        else 
            let s = nextTokenOrEOF()
            if s.Length > 0 then Some(s)
            else next()

    let nextInt() = next() |> Option.map int

    let nextLong() = next() |> Option.map int64

    let rec lines = 
        seq {
            let line = reader.ReadLine()
            if line <> null then 
                yield line
                yield! lines
        }

    let rec tokens = 
        seq {
            if not atEndOfFile then
                let token = nextTokenOrEOF()
                if token.Length > 0 then yield token
                yield! tokens
        }

    let toString (chars: char seq) = 
        chars |> Seq.toArray |> String

    member x.Next() = next()
    member x.NextInt() = nextInt()
    member x.NextLong() = nextLong()
    member x.Lines = lines
    member x.Tokens = tokens
    member x.ToString = toString
