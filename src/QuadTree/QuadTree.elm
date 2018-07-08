module QuadTree.QuadTree exposing (Point2D, QuadTree, extract, fromList, northeast, northwest, southeast, southwest)

-- Exposed Types


type QuadTree
    = QuadTree Data NodeType


type alias Point2D =
    { x : Int, y : Int }



-- Internal Types


type NodeType
    = Empty
    | Leaf Point2D
    | Tree QuadTree QuadTree QuadTree QuadTree


type alias Data =
    { cx : Float
    , cy : Float
    , halfSize : Float
    }



-- Exposed Functions


fromList : List Point2D -> QuadTree
fromList points =
    List.foldl
        insert
        (QuadTree { cx = 0, cy = 0, halfSize = 1 } Empty)
        points


extract : QuadTree -> List Point2D
extract (QuadTree data nodeType) =
    case nodeType of
        Tree nw ne sw se ->
            extract nw
                |> List.append (extract ne)
                |> List.append (extract sw)
                |> List.append (extract se)

        Leaf point ->
            [ point ]

        Empty ->
            []


northwest : QuadTree -> Maybe QuadTree
northwest (QuadTree _ nodeType) =
    case nodeType of
        Tree nw _ _ _ ->
            Just nw

        _ ->
            Nothing


northeast : QuadTree -> Maybe QuadTree
northeast (QuadTree _ nodeType) =
    case nodeType of
        Tree _ ne _ _ ->
            Just ne

        _ ->
            Nothing


southwest : QuadTree -> Maybe QuadTree
southwest (QuadTree _ nodeType) =
    case nodeType of
        Tree _ _ sw _ ->
            Just sw

        _ ->
            Nothing


southeast : QuadTree -> Maybe QuadTree
southeast (QuadTree _ nodeType) =
    case nodeType of
        Tree _ _ _ se ->
            Just se

        _ ->
            Nothing



-- Internal Functions


insert : Point2D -> QuadTree -> QuadTree
insert point (QuadTree data nodeType) =
    if withinBounds data point then
        case nodeType of
            Tree nw ne sw se ->
                insertInSubTree data nw ne sw se point

            Leaf currPoint ->
                insertInLeaf data currPoint point (QuadTree data nodeType)

            Empty ->
                QuadTree data (Leaf point)
    else
        zoomOutAndInsert (QuadTree data nodeType) point


insertInSubTree : Data -> QuadTree -> QuadTree -> QuadTree -> QuadTree -> Point2D -> QuadTree
insertInSubTree data nw ne sw se point =
    let
        ( nw2, ne2, sw2, se2 ) =
            if withinBounds (treeData nw) point then
                ( insert point nw, ne, sw, se )
            else if withinBounds (treeData ne) point then
                ( nw, insert point ne, sw, se )
            else if withinBounds (treeData sw) point then
                ( nw, ne, insert point sw, se )
            else
                ( nw, ne, sw, insert point se )
    in
        QuadTree data (Tree nw2 ne2 sw2 se2)


insertInLeaf : Data -> Point2D -> Point2D -> QuadTree -> QuadTree
insertInLeaf data currPoint point quadTree =
    if currPoint == point then
        quadTree
    else
        let
            nwData =
                { cx = data.cx - data.halfSize / 2, cy = data.cy - data.halfSize / 2, halfSize = data.halfSize / 2 }

            neData =
                { cx = data.cx + data.halfSize / 2, cy = data.cy - data.halfSize / 2, halfSize = data.halfSize / 2 }

            swData =
                { cx = data.cx - data.halfSize / 2, cy = data.cy + data.halfSize / 2, halfSize = data.halfSize / 2 }

            seData =
                { cx = data.cx + data.halfSize / 2, cy = data.cy + data.halfSize / 2, halfSize = data.halfSize / 2 }

            splitTree =
                if toFloat currPoint.x <= data.cx then
                    if toFloat currPoint.y <= data.cy then
                        QuadTree
                            data
                            (Tree
                                (QuadTree nwData (Leaf currPoint))
                                (QuadTree neData Empty)
                                (QuadTree swData Empty)
                                (QuadTree seData Empty)
                            )
                    else
                        QuadTree
                            data
                            (Tree
                                (QuadTree nwData Empty)
                                (QuadTree neData Empty)
                                (QuadTree swData (Leaf currPoint))
                                (QuadTree seData Empty)
                            )
                else if toFloat currPoint.y < data.cy then
                    QuadTree
                        data
                        (Tree
                            (QuadTree nwData Empty)
                            (QuadTree neData (Leaf currPoint))
                            (QuadTree swData Empty)
                            (QuadTree seData Empty)
                        )
                else
                    QuadTree
                        data
                        (Tree
                            (QuadTree nwData Empty)
                            (QuadTree neData Empty)
                            (QuadTree swData Empty)
                            (QuadTree seData (Leaf currPoint))
                        )
        in
            insert point splitTree


withinBounds : Data -> Point2D -> Bool
withinBounds data point =
    let
        xDiff =
            toFloat point.x - data.cx

        yDiff =
            toFloat point.y - data.cy
    in
        (xDiff > (-1 * data.halfSize) && xDiff <= data.halfSize)
            && (yDiff > (-1 * data.halfSize) && yDiff <= data.halfSize)


treeData : QuadTree -> Data
treeData (QuadTree data _) =
    data


zoomOutAndInsert : QuadTree -> Point2D -> QuadTree
zoomOutAndInsert quadTree point =
    let
        data =
            treeData quadTree

        zoomedOutTree =
            if toFloat point.x < data.cx + data.halfSize then
                if toFloat point.y < data.cy + data.halfSize then
                    QuadTree
                        { cx = data.cx - data.halfSize, cy = data.cy - data.halfSize, halfSize = data.halfSize * 2 }
                        (Tree
                            (QuadTree { data | cx = data.cx - data.halfSize * 2, cy = data.cy - data.halfSize * 2 } Empty)
                            (QuadTree { data | cx = data.cx, cy = data.cy - data.halfSize * 2 } Empty)
                            (QuadTree { data | cx = data.cx - data.halfSize * 2, cy = data.cy } Empty)
                            quadTree
                        )
                else
                    QuadTree
                        { cx = data.cx - data.halfSize, cy = data.cy + data.halfSize, halfSize = data.halfSize * 2 }
                        (Tree
                            (QuadTree { data | cx = data.cx - data.halfSize * 2, cy = data.cy } Empty)
                            quadTree
                            (QuadTree { data | cx = data.cx - data.halfSize * 2, cy = data.cy + data.halfSize * 2 } Empty)
                            (QuadTree { data | cx = data.cx, cy = data.cy + data.halfSize * 2 } Empty)
                        )
            else if toFloat point.y < data.cy + data.halfSize then
                QuadTree
                    { cx = data.cx + data.halfSize, cy = data.cy - data.halfSize, halfSize = data.halfSize * 2 }
                    (Tree
                        (QuadTree { data | cx = data.cx, cy = data.cy - data.halfSize * 2 } Empty)
                        (QuadTree { data | cx = data.cx + data.halfSize * 2, cy = data.cy - data.halfSize * 2 } Empty)
                        quadTree
                        (QuadTree { data | cx = data.cx + data.halfSize * 2, cy = data.cy } Empty)
                    )
            else
                QuadTree
                    { cx = data.cx + data.halfSize, cy = data.cy + data.halfSize, halfSize = data.halfSize * 2 }
                    (Tree
                        quadTree
                        (QuadTree { data | cx = data.cx + data.halfSize * 2, cy = data.cy } Empty)
                        (QuadTree { data | cx = data.cx, cy = data.cy + data.halfSize * 2 } Empty)
                        (QuadTree { data | cx = data.cx + data.halfSize * 2, cy = data.cy + data.halfSize * 2 } Empty)
                    )
    in
        insert point zoomedOutTree
