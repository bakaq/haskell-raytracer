import System.IO

-- Vector stuff
data Vec3 = Vec3 Double Double Double deriving (Show, Eq)

-- Dot product
(***) :: Vec3 -> Vec3 -> Double
Vec3 a1 a2 a3 *** Vec3 b1 b2 b3 = (a1*b1) + (a2*b2) + (a3*b3)

(|+|) :: Vec3 -> Vec3 -> Vec3
(Vec3 a1 a2 a3) |+| (Vec3 b1 b2 b3) = Vec3 (a1+b1) (a2+b2) (a3+b3)

(|-|) :: Vec3 -> Vec3 -> Vec3
(Vec3 a1 a2 a3) |-| (Vec3 b1 b2 b3) = Vec3 (a1-b1) (a2-b2) (a3-b3)

(|*) :: Double -> Vec3 -> Vec3
a |* (Vec3 vx vy vz) = Vec3 (a*vx) (a*vy) (a*vz)
(*|) :: Vec3 -> Double -> Vec3
v *| a = a |* v

vLength :: Vec3 -> Double
vLength (Vec3 vx vy vz) = sqrt (vx**2 + vy**2 + vz**2)

normalized :: Vec3 -> Vec3
normalized v = v *| (1/(vLength v))

type Position = Vec3
type Direction = Vec3

-- Meshs
data Ray = Ray Position Direction
data Mesh = Sphere {radius :: Double, position :: Position}

meshList = [Sphere {radius=1, position=(Vec3 0 0 (-5))}]

intersect :: Ray -> Mesh -> Maybe Vec3
intersect (Ray rayOrig rayDir) (Sphere {radius=r, position=pos}) =
    if toTang <= r
        then Just (Vec3 0 0 0)
        else Nothing
    where 
        toTang = vLength (tang |-| pos)
        tang = rayOrig |+| (rayDir *| (toSphere *** rayDir))
        toSphere = pos |-| rayOrig

-- Color and image stuff
data Color = Color Int Int Int deriving (Show)
data Shape = Shape {width :: Int, height :: Int} deriving (Show)
data Image = Image {shape :: Shape, raw :: [Color]} deriving (Show)

-- Turns an image into a PPM ascii file
toPPM :: Image -> String
toPPM img = header ++ toData img
    where header = "P3\n" ++ (show $ width $ shape img) ++
                    " " ++ (show $ height $ shape img) ++ "\n255\n"
          toData Image {raw=rawData} =
              foldl (\a b -> a  ++ b ++ "\n") "" (map flatColor rawData)
              where flatColor (Color r g b) =
                        (show r) ++ " " ++ (show g) ++ " " ++ (show b)

-- Render stuff
renderRay :: Ray -> Color
renderRay ray =
    let foldedIntersections = foldl propJust Nothing intersections
        propJust a b = case a of
            Just _ -> a
            Nothing -> b
        intersections = map (intersect ray) meshList
    in
        case foldedIntersections of
            Just _ -> Color 0 0 0 
            Nothing -> Color 255 255 255

renderPixel :: Shape -> (Int, Int) -> Color
renderPixel dimensions (x, y) = 
    renderRay ray
    where ray = Ray (Vec3 0 0 0) rayDir
           where rayDir = normalized pixelPos
                  where pixelPos = Vec3 px py (-near)
                         where near = -0.1
                               px = (screenw * (dx/dimx) - (screenw/2))
                                 where screenw = 0.1
                                       dimx = fromIntegral $ width dimensions
                                       dx :: Double
                                       dx = fromIntegral x
                               py = -(screenh * (dy/dimy) - (screenh/2))
                                 where screenh = 0.1
                                       dimy = fromIntegral $ height dimensions
                                       dy :: Double
                                       dy = fromIntegral y


renderScreen :: Shape -> Image
renderScreen dimensions =
    Image {shape=dimensions,raw=rawImageTo (w*h)}
    where 
        Shape {width=w, height=h} = dimensions
        rawImageTo 0 = []
        rawImageTo n = rawImageTo (n-1) ++ [renderPixel dimensions (n `mod` h, n `div` h)]

main = do
    writeFile "image.ppm" $ toPPM $ renderScreen $ Shape {width=64, height=64}
