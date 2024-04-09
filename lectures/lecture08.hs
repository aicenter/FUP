-- Lecture 9 - Haskell types

-- Lambda abstractions
f :: Int -> Int -> Int -> Int
--f x y z = x + y * z
--f x y = \z -> x + y * z
--f x = \y -> (\z -> x + y * z) 
f x = \y z -> x + y * z
--f = \x ->(\y -> (\z -> x + y * z))

-- sections
a = (2/) 1
b = (/2) 1
pxs = filter (>0) [-1,0,2,-3,1]

-- parametric polymorphisms
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- ad hoc polymorphism
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater
    where smaller = [y | y <- xs, y <= x]
          greater = [y | y <- xs, y > x]

-- type
type Pos = (Int,Int)
left :: Pos -> Pos
left (x,y) = (x-1,y)

-- parametric type
type Pair a = (a,a)
mult :: Pair Int -> Int
mult (m,n) = m*n

copy :: a -> Pair a
copy x = (x,x)

-- algebraic data type
data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes,No,Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float deriving (Eq, Show)

area :: Shape -> Float
area (Circle r) = pi * r**2
area (Rect x y) = x * y

-- records
data Person = Person { firstName :: String,
                        lastName :: String,
                             age :: Int,
                           phone :: String,
                         address :: String } deriving Show

defaultPerson :: Person
defaultPerson = Person { firstName="John", lastName="Doe", age=40, phone="777666555", address="Prague"}
olderJohn :: Person
olderJohn = defaultPerson{age = 70}

-- recursive algebraic data type
data List a = Nil | Cons a (List a) deriving Eq

{-
rev :: List a -> List a
rev lst = iter lst Prazdny where
    iter Prazdny acc = acc
    iter (Spoj x l) acc = iter l (Spoj x acc)
-}

instance Show a => Show (List a) where
    show lst = "<" ++ disp lst ++ ">" where
        disp Nil = ""
        disp (Cons x Nil) = show x
        disp (Cons x l) = show x ++ ", " ++ disp l


-- arithmetic expressions
data Expr a = Val a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)

instance Show a => Show (Expr a) where
    show (Val x) = show x
    show (Add e1 e2) = "(" ++ show e1
                           ++ " + "
                           ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1
                           ++ " * "
                           ++ show e2 ++ ")"

eval :: (Num a) => Expr a -> a
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

e :: Expr Int
e = Add (Val 3) (Mul (Val 5) (Val 8))

instance (Ord a, Num a) => Num (Expr a) where
    x + y         = Add x y
    x - y         = Add x (Mul (Val (-1)) y)
    x * y         = Mul x y
    negate x      = Mul (Val (-1)) x
    abs x         | eval x >= 0 = x
                  | otherwise = negate x
    signum        = Val . signum . eval
    fromInteger = Val . fromInteger

instance Num b => Num (a -> b) where
    f + g = \x -> f x + g x
    f - g = \x -> f x - g x
    f * g = \x -> f x * g x
    negate f = \x -> -(f x)
    abs f = abs . f
    signum f = 1
    fromInteger n = const (fromInteger n)

-- instance (Show b, Num a) => Show (a->b) where
--     show f = "Function Num a to Num a mapping 0->" 
--              ++ show (f 0) ++ " and 1->" ++ show (f 1)
