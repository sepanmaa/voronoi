module Voronoi exposing (voronoi, delaunay)

type alias Point = (Int, Int)

type alias Triangle = (Point, Point, Point)

    
circumcenter : Triangle -> Point
circumcenter ((ax,ay), (bx,by), (cx,cy)) =
    let (ax_, ay_) = (0, 0)
        (bx_, by_) = (bx-ax, by-ay)
        (cx_, cy_) = (cx-ax, cy-ay)
        d = toFloat (2 * (bx_ * cy_ - by_ * cx_))
        ux = toFloat ((cy_ * (bx_^2 + by_^2) - by_ * (cx_^2 + cy_^2))) / d
        uy = toFloat ((bx_ * (cx_^2 + cy_^2) - cx_ * (bx_^2 + by_^2))) / d
    in (ax + round ux, ay + round uy)


inCircumcircle : Point -> Triangle -> Bool
inCircumcircle (x, y) t =
    let (cx, cy) = circumcenter t
        ((ax, ay), _, _) = t
        radius = (ax-cx)^2 + (ay-cy)^2
        pointDistance = (x-cx)^2 + (y-cy)^2
    in pointDistance < radius


edges : Triangle -> List (Point, Point)
edges ((ax, ay), (bx, by), (cx, cy)) =
    [ ((ax, ay), (bx, by))
    , ((bx, by), (cx, cy))
    , ((cx, cy), (ax, ay))
    , ((bx, by), (ax, ay))
    , ((cx, cy), (bx, by))
    , ((ax, ay), (cx, cy))
    ]


polygonEdges : List (Triangle) -> List (Point, Point)
polygonEdges ts =
    let hasEdge e t = List.any ((==) e) (edges t) 
        unshared e = List.length (List.filter (hasEdge e) ts) == 1
    in List.foldr (\t es -> (List.filter unshared (edges t)) ++ es) [] ts

            
sort : Triangle -> Triangle
sort (a, b, c) =
    case List.sort [a, b, c] of
        (a_::b_::c_::[]) -> (a_, b_, c_)
        _ -> (a, b, c)
             

unique : List a -> List a
unique ts =
    List.foldr (\t ts_ -> t :: List.filter ((/=) t) ts_) [] ts

            
convexCompare : Point -> Point -> Point -> Order
convexCompare (x, y) (x2, y2) (x3, y3) =
    let a = (atan2 (toFloat (y-y2)) (toFloat (x-x2)))
        b = (atan2 (toFloat (y-y3)) (toFloat (x-x3)))
    in compare a b


hasVertex : Point -> Triangle -> Bool
hasVertex p (a, b, c) = p == a || p == b || p == c


-- bowyer-watson algorithm
delaunay : List (Point) -> Triangle -> List (Triangle)
delaunay ps superTriangle =
    List.foldr
        (\p ts ->
             let (badTriangles, triangulation) = List.partition (inCircumcircle p) ts
                 makeTriangles = List.foldr (\(a, b) ts_ -> sort (p, a, b) :: ts_) []
                 newTriangles = unique <| makeTriangles (polygonEdges badTriangles)
             in triangulation ++ newTriangles
        ) [superTriangle] <| unique ps                                              

                                
voronoi : List (Point) -> (Int, Int) -> List (List (Point))
voronoi ps (w, h) =
    let del = delaunay ps ((-w, -2*h), (4*w, 2*h), (-w, 2*h))
    in List.foldr (\p polys ->
                       (List.sortWith (convexCompare p)
                        <| List.map circumcenter
                        <| List.filter (hasVertex p) del) :: polys) [] ps
