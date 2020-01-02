// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace GameOfLife

open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module Models =
    type Location =
        { X: int
          Y: int }
    type State =
        | Alive
        | Dead
    type PositionedCell =
        { State: State
          Location: Location }
    type World = PositionedCell list

module App =
    open Models
    type Msg =
        | LoadedGame

    type Model =
        { Dimension: int
          World: World }

        static member EmptyWord dimension =
            { Dimension = dimension
              World = [] }

    let init () =
         Model.EmptyWord 10 , Cmd.none

    let update msg model =
        match msg with
        | LoadedGame ->
            model , Cmd.none

    let view model dispatch =

        let loadingView =
            View.ActivityIndicator(
                color = Color.LightBlue, isRunning = true, verticalOptions = LayoutOptions.CenterAndExpand,
                created = (fun effect -> effect.Color <- Color.White))

        let gridContent dimension =
             View.Grid(
                rowdefs= [ for _ in 1 ..dimension -> Dimension.Star],
                coldefs = [ for _ in 1 .. dimension -> Dimension.Star],
                children = [
                   for i in 1 .. dimension do
                        for j in 1 .. dimension ->
                            View.BoxView(Color.White).Row(i-1).Column(j-1) ],
                margin = Thickness(8.0))

        View.NavigationPage(
            pages = [
                View.ContentPage(title = "Game of Life",
                    useSafeArea = true,
                    content =
                        View.StackLayout(
                            backgroundColor = Color.Gray,
                                children = [
                                    gridContent model.Dimension
                                    View.StackLayout(
                                            children = [
                                                View.Button(text = "Play", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(5.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0)
                                                View.Button(text = "Reset", horizontalOptions = LayoutOptions.CenterAndExpand,
                                                    margin = Thickness(0.0, 20.0), borderColor = Color.Green, borderWidth = 1.0, width = 100.0 )
                                            ],
                                            orientation = StackOrientation.Horizontal, backgroundColor = Color.White,
                                            verticalOptions = LayoutOptions.EndAndExpand)]))])

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


