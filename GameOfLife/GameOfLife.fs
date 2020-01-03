namespace GameOfLife

open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open GameOfLife.Models
open System.Timers

module App =

    type Msg =
        | Seed of State list
        | TimerTick
        | TogglePause
        | NewWorld
        | ToggleState of PositionedCell
        | ResetCells

    let generateRandomList dimension =
        List.init dimension (fun _ -> System.Random().Next(2))

    let randomStateGenerator dimension =
        let stateList =
            generateRandomList (dimension * dimension)
            |> List.map (fun c ->
                match c with
                | 1 -> Alive
                | _ -> Dead)
        Seed stateList

    let init () =
         Model.EmptyWord 10 , Cmd.none

    let update msg model =
        match msg with
        | Seed states ->
            let newModel = initModel model states
            newModel, Cmd.none

        | TimerTick ->
            match model.Paused with
            | true -> model, Cmd.none
            | _ -> tick model, Cmd.ofMsg (randomStateGenerator  model.Dimension)

        | TogglePause ->
            { model with Paused = not model.Paused }, Cmd.none

        | NewWorld ->
            model, Cmd.ofMsg (randomStateGenerator model.Dimension)

        | ToggleState cell ->
            toggleState model cell, Cmd.none

        | ResetCells ->
            killCells model, Cmd.none
    let view (model: Model) dispatch =

        let createRow cells rowNumber =
            cells
            |> List.filter (fun c -> c.Location.X = rowNumber)
            |> List.sortBy (fun c -> c.Location.Y)
            |> List.map(fun cell ->
                             let cellState =
                                match cell.State with
                                | Alive -> "👾"
                                | Dead -> "👻"
                             View.Button(text = cellState))

        let gameContent model =

            let nbRows =
                if model.World.Length > 0 then
                        model.World
                        |> List.map (fun c -> c.Location.X)
                        |> List.max
                    else
                        0

            let elements = createRow model.World nbRows

            View.Grid(
                rowdefs= [for _ in 1 .. nbRows -> Dimension.Auto],
                coldefs = [ for _ in 1 .. nbRows -> Dimension.Star],
                children = [
                    for i in 1 .. nbRows do
                        for j in 1 .. nbRows ->
                            let element = (elements.Item (i-1))
                            element.Row(i-1).Column(j-1)
                ],
                margin = Thickness(8.0), verticalOptions = LayoutOptions.CenterAndExpand)

        let pauseMessage =
            match model.Paused with
            | true  ->  "Unpause"
            | _ -> "Pause"

        let dimensionText =  "X " + model.Dimension.ToString()

        View.NavigationPage(
            pages = [
                View.ContentPage(title = "Game of Life",
                    useSafeArea = true,
                    content =
                        View.StackLayout(
                            backgroundColor = Color.Gray,
                                children = [
                                    View.Label(text= dimensionText,
                                               fontSize = FontSize 20.,
                                               fontAttributes = FontAttributes.Bold,
                                               textColor = Color.White,
                                               horizontalOptions =LayoutOptions.CenterAndExpand,
                                               verticalOptions = LayoutOptions.EndAndExpand)
                                    gameContent model
                                    View.StackLayout(
                                        children = [
                                                View.Button(text = pauseMessage, horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    command = (fun _ -> dispatch TogglePause),
                                                    margin = Thickness(5.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0)
                                                View.Button(text = "Reset", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    command = (fun _ -> dispatch ResetCells),
                                                    margin = Thickness(0.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0 )
                                                View.Button(text = "New", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    command = (fun _ -> dispatch NewWorld),
                                                    margin = Thickness(0.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0 )
                                            ],
                                            orientation = StackOrientation.Horizontal, backgroundColor = Color.White,
                                            verticalOptions = LayoutOptions.EndAndExpand)
                                    ]
                                )
                        )
                ]
            )
    let timerTick dispatch =
        let timer = new Timer(1000.)
        timer.Elapsed.Subscribe (fun _ -> dispatch TimerTick) |> ignore
        timer.Enabled <- true
        timer.Start()

    let program = Program.mkProgram init update view

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
        |> Program.withSubscription (fun _ -> Cmd.ofSub  App.timerTick)
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    do runner.EnableLiveUpdate()
#endif