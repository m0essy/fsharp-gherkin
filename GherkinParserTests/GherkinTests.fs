module GherkinTests
    open Xunit
    open Gherkin

    let [<Fact>] ``parsing feature file without scenarios``() : unit = 
        let feature = parseGherkin "Feature: some feature name"

        match feature with
            | Some(Feature(name, _)) -> Assert.Equal("some feature name", name) |> ignore
            | _ -> Assert.True(false) |> ignore

    let [<Fact>] ``parsing feature file with a scenario``() : unit = 
        let contents = "Feature: some feature
            Scenario: scenario name"

        let feature = parseGherkin contents
        match feature with
            | Some(Feature(_, scenario::_)) -> 
                match scenario with 
                | Scenario(name, _) -> Assert.Equal("scenario name", name)
            | _ -> Assert.True(false)

    let [<Fact>] ``parsing scenario with steps``() : unit = 
        let contents = "Scenario: scenario name
        Given some precondition
        When doing something
        Then there must be some outcome"

        let scenario = parseScenario contents
        Assert.Equal(
            Some(Scenario("scenario name", 
                    [Given(StepContents "some precondition");
                    When(StepContents "doing something");
                    Then(StepContents "there must be some outcome")])),
            scenario)

    let parseOneStep = parseStep >> Option.bind (Some << List.head)

    let [<Fact>] ``parsing a step``() : unit = 
        Assert.Equal(Some(Given(StepContents "some precondition")), parseOneStep "Given some precondition")
        Assert.Equal(Some(When(StepContents "some action")), parseOneStep "When some action")
        Assert.Equal(Some(Then(StepContents "some outcome")), parseOneStep "Then some outcome")

    let [<Fact>]``parsing scenario witition"), step)and``() : unit = 
        let contents = "Scenario: scenario name
        Given some precondition
        And other precondition
        And again other precondition
        When doing something
        Then there must be some outcome
        And also another effect"

        let scenario = parseScenario contents
        Assert.Equal(
            Some(Scenario("scenario name", 
                    [Given(StepContents "some precondition");
                    Given(StepContents "other precondition");
                    Given(StepContents "again other precondition");
                    When(StepContents "doing something");
                    Then(StepContents "there must be some outcome");
                    Then(StepContents "also another effect")])),
            scenario)
