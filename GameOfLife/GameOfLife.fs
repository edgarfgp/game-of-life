namespace GameOfLife

open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module Models =

    type State =
        | Alive
        | Dead

    type Location =
        { X: int
          Y: int }

    type PositionedCell =
        { State: State
          Location: Location }

    type Model =
        { Dimension: int
          World: PositionedCell list
          Paused: bool }

    let initModel model states =
        let allCoordinates =
            [ 1 .. model.Dimension ]
            |> List.map (fun x ->
                [ 1 .. model.Dimension ]
                |> List.map (fun y ->
                    { X = x
                      Y = y }))
            |> List.concat

        let cells =
            allCoordinates
            |> List.zip states
            |> List.map (fun (state, coords) ->
                { Location = coords
                  State = state })

        { model with World = cells }

    let generateRandomList dimension =
        List.init dimension (fun _ -> System.Random().Next(1, 2))

    let findNeighboursFor coordinates cells =
        let deltas =
            [ (-1, 1)
              (0, 1)
              (1, 1)
              (-1, 0)
              (1, 0)
              (-1, -1)
              (0, -1)
              (1, -1) ]

        let coordinatesToCheck =
            deltas
            |> List.map (fun (dx, dy) ->
                { X = coordinates.X + dx
                  Y = coordinates.Y + dy })

        cells
        |> List.filter (fun cell ->
            let result = coordinatesToCheck
            result |> List.exists (fun a -> a.X = cell.Location.X && a.Y = cell.Location.Y))

    let applyRules nbLivingNeighbours cell =
        let newState =
            match cell.State with
            | Dead ->
                match nbLivingNeighbours with
                | 3 -> Alive
                | _ -> Dead

            | Alive ->
                match nbLivingNeighbours with
                | 2 -> Alive
                | 3 -> Alive
                | _ -> Dead

        { cell with State = newState }

    let tickCell cell world =
        let neighbours = findNeighboursFor cell.Location world

        let nbLivingNeighbours =
            neighbours
            |> List.filter (fun cellState -> cellState.State = Alive)
            |> List.length
        applyRules nbLivingNeighbours cell

    let tick model =
        { model with World = model.World |> List.map (fun cell -> tickCell cell model.World) }

    let flip state =
        match state with
        | Alive ->
            Dead
        | Dead ->
            Alive

    let toggleState model cell =
        let toggled = { cell with State = (flip cell.State) }

        let others =
            model.World |> List.filter (fun c -> c.State <> cell.State)

        { model with World = toggled :: others }

    let kill cell =
        { cell with State = Dead }

    let killCells model =
        { model with World = model.World |> List.map kill }


module App =

    open Models
    type Msg =
        | Seed of State list
        | TimerTick
        | TogglePause
        | NewWorld
        | ToggleState of PositionedCell
        | KillCells

    let randomStateGenerator dimension =
        let stateList =
            generateRandomList (dimension * dimension)
            |> List.map (fun c ->
                match c with
                | 1 -> Alive
                | _ -> Dead)
        Seed stateList

    let emptyWord dimension =
            { Dimension = dimension
              World = []
              Paused = false }

    let init () =
         emptyWord 10 , Cmd.ofMsg (randomStateGenerator 10)

    let update msg model =
        match msg with
        | Seed states ->
            (initModel model states), Cmd.none

        | TimerTick ->
            if model.Paused then
                ( model, Cmd.none )
            else
                ( tick model, Cmd.none )

        | TogglePause ->
            { model with Paused = not model.Paused }, Cmd.none

        | NewWorld ->
            model, Cmd.ofMsg (randomStateGenerator model.Dimension)

        | ToggleState cell ->
            (toggleState model cell), Cmd.none

        | KillCells ->
            ( killCells model, Cmd.none )

    let view model _ =

        let createCell cell =
            let (state, color ) =
                    match cell.State with
                        | Alive ->
                            ( "👾", Color.White )

                        | Dead ->
                            ( "👻", Color.Gray )

            View.Button(text = state, backgroundColor = color)

        let createRow cells rowNumber =
            cells
            |> List.filter (fun c -> c.Location.X = rowNumber)
            |> List.sortBy (fun c -> c.Location.Y)
            |> List.map createCell

        let gameContent model =

            let nbRows =
                if model.World.Length > 0 then
                        model.World
                        |> List.map (fun c -> c.Location.X)
                        |> List.max
                    else
                        0

            let result = (createRow model.World nbRows)

            View.Grid(
                rowdefs= [for _ in 1 .. nbRows -> Dimension.Star],
                coldefs = [ for _ in 1 .. nbRows -> Dimension.Star],
                children = [
                    for i in 1 .. nbRows do
                        for j in 1 .. nbRows ->
                            let item = (result.Item 0)
                            item.Row(i-1).Column(j-1)
                ],
                margin = Thickness(8.0))

        let pauseMessage =
            match model.Paused with
            | true  ->  "Unpause"
            | _ -> "Pause"

        View.NavigationPage(
            pages = [
                View.ContentPage(title = "Game of Life",
                    useSafeArea = true,
                    content =
                        View.StackLayout(
                            backgroundColor = Color.Gray,
                                children = [

                                    gameContent model

                                    View.StackLayout(
                                        children = [
                                                View.Button(text = pauseMessage, horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(5.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0)

                                                View.Button(text = "Reset", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(0.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0 )

                                                View.Button(text = "New", horizontalOptions = LayoutOptions.CenterAndExpand,
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
        let timer = new System.Timers.Timer(1.)
        timer.Elapsed.Subscribe (fun _ -> dispatch TimerTick) |> ignore
        timer.Enabled <- true
        timer.Start()

    let program = Program.mkProgram init update view

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        //|> Program.withSubscription (fun _ -> Cmd.ofSub  App.timerTick)
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    do runner.EnableLiveUpdate()
#endif


