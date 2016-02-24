{-# LANGUAGE ExistentialQuantification #-}

-- | Define a general class defining what each instantiating class should implement
class Shape_ a where
    perimeter :: a -> Double
    area      :: a -> Double


-- | Create a `Shape` "type box" for any types that instantiate the `Shape_` class. 
data Shape = forall a. Shape_ a => Shape a


-- | Create aliases for better readablility
type Radius = Double
type Side   = Double


-- | Define some data types that can implement the `Shape_` class
data Circle    = Circle Radius       deriving (Show)
data Rectangle = Rectangle Side Side deriving (Show)
data Square    = Square Side         deriving (Show)


-- | Provide implementations for each function described by the `Shape_` typeclass
--      One for `Circle`, `Rectangle`, and `Sqaure`
instance Shape_ Circle where
    perimeter (Circle radius) = 2 * pi * radius
    area      (Circle radius) = pi * radius * radius

instance Shape_ Rectangle where
    perimeter (Rectangle width height) = 2 * (width + height)
    area      (Rectangle width height) = width * height

instance Shape_ Square where
    perimeter (Square side) = 4 * side
    area      (Square side) = side * side


-- | Tell the `Shape` data type how to call the functions of a data type that
---     instantiates `Shape_`
instance Shape_ Shape where
    perimeter (Shape shape) = perimeter shape
    area      (Shape shape) = area shape


-- Wrap the Shape constructors in a function for easier creation
circle :: Radius -> Shape
circle radius = Shape (Circle radius)

rectangle :: Side -> Side -> Shape
rectangle width height = Shape (Rectangle width height)

square :: Side -> Shape
square side = Shape (Square side)


-- | Print out the perimeters/areas of the shapes, all contained in the same list
main = do
    let test = [circle 3, rectangle 4 5, square 6]
    putStrLn ""
    putStrLn $ "perimeters: " ++ show (map perimeter test)
    putStrLn ""
    putStrLn $ "areas     : " ++ show (map area test)