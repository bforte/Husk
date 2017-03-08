import Data.Function (fix)
import System.Environment (getArgs)
import Data.Char (ord)

class (Show a, Read a, Eq a, Ord a) => Concrete a where
  isTruthy :: a -> Bool

instance Concrete Integer where
  isTruthy = (/= 0)
 
instance Concrete Char where
  isTruthy = (/= 0).ord

instance Concrete a => Concrete [a] where
  isTruthy = (/= [])

boolToNum :: (Num a) => Bool -> a
boolToNum = fromInteger . toInteger . fromEnum

func_fix :: (a -> a) -> a
func_fix = fix

func_fixp :: Concrete a => (a -> a) -> a -> a
func_fixp f a = go a $ f a
  where go x y | x == y    = y
               | otherwise = go y $ f y

func_app :: (a -> b) -> a -> b
func_app = id

func_com :: (b -> c) -> (a -> b) -> a -> c
func_com = (.)

func_com2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
func_com2 f g x = f . g x

func_com3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
func_com3 f g x y = f . g x y

func_add :: Integer -> Integer -> Integer
func_add = (+)

func_sub :: Integer -> Integer -> Integer
func_sub  = (-)

func_neg :: Integer -> Integer
func_neg x = -x

func_mul :: Integer -> Integer -> Integer
func_mul = (*)

func_pure :: a -> [a]
func_pure = (: [])

func_cons :: a -> [a] -> [a]
func_cons = (:)

func_snoc :: [a] -> a -> [a]
func_snoc x y = x ++ [y]

func_cat :: [a] -> [a] -> [a]
func_cat = (++)

func_pair :: a -> a -> [a]
func_pair x y = [x, y]

func_map :: (a -> b) -> [a] -> [b]
func_map = map

func_zip :: (a -> b -> c) -> [a] -> [b] -> [c]
func_zip = zipWith

func_lt :: Concrete a => a -> a -> Integer
func_lt x y = boolToNum $ x < y

func_gt :: Concrete a => a -> a -> Integer
func_gt x y = boolToNum $ x > y

func_eq :: Concrete a => a -> a -> Integer
func_eq x y = boolToNum $ x == y

func_if :: Concrete a => a -> b -> b -> b
func_if a b c = if isTruthy a then b else c
