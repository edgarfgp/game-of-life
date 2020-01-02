// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace GameOfLife

open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open GameOfLife.Models
open Xamarin.Forms
open System

module App =
    type Msg =
        | LoadedGame

    type Model  =
        { Game : Game
          GameLoafing : bool }

    let init () =
        { Game = { Columns = 10 ; Rows = 10 } ; GameLoafing = true }, Cmd.ofMsg LoadedGame

    let stackLayoutRef = ViewRef<StackLayout>()

    let update msg model =
        match msg with
        | LoadedGame ->
            match stackLayoutRef.TryValue with
            | None  -> { model with GameLoafing = true }, Cmd.none
            | Some abs ->
                let numcolumns= int (Math.Round((abs.Width / float 30)))
                let numRows = int (Math.Round((abs.Height / float 30)))
                match (numcolumns * numRows) > 400  with
                |  true ->
                    let cellSize = int (Math.Sqrt((abs.Width * abs.Height) /  float 400))
                    let cols = int (abs.Width / (float cellSize))
                    let rows = int (abs.Height / float cellSize)

                    { model with Game = { Columns = cols ; Rows = rows }; GameLoafing = false }, Cmd.none

                | false ->
                    { model with Game = { Columns = numcolumns ; Rows = numRows } ; GameLoafing = false }, Cmd.none

    let view model dispatch =

        let loadingView =
            View.ActivityIndicator(
                color = Color.LightBlue, isRunning = true, verticalOptions = LayoutOptions.CenterAndExpand,
                created = (fun effect -> effect.Color <- Color.White))

        let gridContent cells =
             View.Grid(
                rowdefs= [ for _ in 1 .. cells.Rows -> Dimension.Star],
                coldefs = [ for _ in 1 .. cells.Columns -> Dimension.Star],
                children = [
                   for i in 1 .. cells.Rows do
                        for j in 1 .. cells.Columns ->
                            View.BoxView(Color.White).Row(i-1).Column(j-1) ],
                margin = Thickness(8.0))

        let  gameContent =
            match model.GameLoafing with
            | true -> loadingView
            | false -> gridContent model.Game

        View.NavigationPage(
            pages = [
                View.ContentPage(title = "Game of Life",
                    useSafeArea = true,
                    content =
                        View.StackLayout(
                            backgroundColor = Color.Gray,
                                children = [
                                    gameContent
                                    View.StackLayout(
                                            children = [
                                                View.Button(text = "Play", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(5.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0)
                                                View.Button(text = "Reset", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(0.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0 )
                                            ],
                                            orientation = StackOrientation.Horizontal, backgroundColor = Color.White,
                                            verticalOptions = LayoutOptions.EndAndExpand)], ref = stackLayoutRef))])

    let program = Program.mkProgram init update view

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    do runner.EnableLiveUpdate()
#endif


