// Learn more about F# at http://fsharp.net
#light

module Gherkin
    open FParsec
    open FParsec.CharParsers

    type Feature = 
        Feature of string * Scenario list
    and Scenario = 
        Scenario of string * Step list
    and Step = 
          Given of StepContents
        | When of StepContents
        | Then of StepContents
    and StepContents = 
          StepContents of string

    module Parsers = 
        let tillEndOfLine : Parser<string, unit> = 
            many1Chars (noneOf "\n") .>> optional (pchar '\n')

        let restStepGroup (createStep: StepContents -> Step) : Parser<Step list, unit> = 
            many (attempt (spaces >>. pstring "And " >>. tillEndOfLine |>> (StepContents >> createStep)))

        let stepParser (createStep: StepContents -> Step) : Parser<Step list, unit> = 
            (tillEndOfLine |>> (StepContents >> createStep)) 
            .>>. restStepGroup createStep
            |>> List.Cons
            
        let stepGroupParser : Parser<Step list, unit> = 
            spaces >>. 
            ((pstring "Given " >>. stepParser Given)
            <|> (pstring "When " >>. stepParser When)
            <|> (pstring "Then " >>. stepParser Then))

        let scenarioParser : Parser<Scenario, unit> = 
            spaces >>. pstring "Scenario: " >>. tillEndOfLine
            .>>. many stepGroupParser
            |>> (fun (name, steps) -> Scenario (name, List.concat steps))

        let featureParser : Parser<Feature, unit> = 
            pstring "Feature: " 
            >>. tillEndOfLine
            .>>. many scenarioParser
            |>> Feature

    let defineParse (parser: Parser<'a, unit>) (contents:string) = 
        let result = FParsec.CharParsers.run parser contents    
        match result with 
        | Success(result, _, _) -> Some(result)
        | _ -> None

    let parseScenario = defineParse Parsers.scenarioParser
    let parseGherkin = defineParse Parsers.featureParser
    let parseStep = defineParse Parsers.stepGroupParser
    