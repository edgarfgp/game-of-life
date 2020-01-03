namespace GameOfLife

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

        static member EmptyWord dimension =
            { Dimension = dimension
              World = []
              Paused = false }

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
            coordinatesToCheck
            |> List.exists (fun a -> a.X = cell.Location.X && a.Y = cell.Location.Y))

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


