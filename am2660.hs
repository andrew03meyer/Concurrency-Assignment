import Data.Ratio
import Data.Maybe
import Control.Concurrent
import Data.Text.Lazy.Builder.RealFloat (FPFormat(Fixed))
import Data.Fixed (Fixed)

tobedone :: whatever
tobedone = error "this still needs to be done"


data Interval = Interval { low :: Rational, high:: Rational }
    -- deriving Show

-- Q1
mkInterval :: Rational -> Rational -> Interval
mkInterval x y = if x > y then Interval y x else Interval x y

-- Q2             
ilength :: Interval -> Rational
ilength (Interval x y)= y-x

-- Q3
midpoint :: Interval -> Rational
midpoint (Interval x y) = (y+x) / 2

-- Q4
isin :: Rational -> Interval -> Bool
isin n (Interval x y)= n >= x && n < y

-- Q5
scale10 :: Interval -> Interval
scale10 (Interval x y) = mkInterval (x*10) (y*10)

data FixedPointNumber = FixedPointNumber { mantissa :: Integer, shift :: Int }

-- Q6
shift1 :: FixedPointNumber -> FixedPointNumber
shift1 (FixedPointNumber x y) = FixedPointNumber x (y+1)

-- Q7               
instance Show FixedPointNumber where
    show (FixedPointNumber x 0) = show x
    show (FixedPointNumber x y)
        | y > length (show x) = "0." ++ replicate (y - length (show x)) '0' ++ show x
        | otherwise = take (length (show x) - y) (show x) ++ "." ++ drop (length (show x) - y) (show x)

-- Q8  
decimalInfo :: Interval -> Maybe FixedPointNumber
decimalInfo x
    | ilength x == 0 = Nothing
    | otherwise = Just (recursion x)
    where
        recursion x
            | fromIntegral (floor (fromRational (midpoint x))) == midpoint x = FixedPointNumber (floor (fromRational (midpoint x))) 0
            | otherwise = shift1 (fromJust (decimalInfo (scale10 x)))

-- Q9       
instance Show Interval where
    show = hackedshow
    --     | ilength i == 0 = show (midpoint i)
    --     | otherwise = show (decimalInfo i)

-- if you struggle to make this work, use the hack below
-- that works if the precision is not too egregious
-- and the bounds are not too far apart
rat2double :: Rational -> Double
rat2double = fromRational

hackedshow :: Interval -> String
hackedshow i | low i/=high i
             = commons (show (rat2double (low i))) (show (rat2double (high i)))
             | otherwise = show (low i)
               where
               commons (x:xs)(y:ys) | x==y
                                    = x:commons xs ys
                                    | otherwise
                                    = [y]
               commons [] (y:ys)    = [y]
               commons [] []        = []
               commons _  _         = error "bounds not right"
-- Q10

sqrtAlgo :: Rational -> Interval -> Rational -> Interval
sqrtAlgo x y z
    | ilength y < z = y
    | otherwise = sqrtAlgo x (rootInterval x y) z
    where
        rootInterval :: Rational -> Interval -> Interval
        rootInterval x (Interval a b) = mkInterval (midpoint (mkInterval a b)) (x / midpoint (mkInterval a b))


class PerfectSqrt a
    where
    perfectSqrt :: a -> Maybe a

-- Q11
instance PerfectSqrt Integer where
    perfectSqrt n
        | root*root == n = Just root
        | otherwise = Nothing
        where
            root = ceiling (low (sqrtAlgo (fromInteger n) (mkInterval 0 (fromInteger n)) 1))

-- Q12  
-- this is effectively the instance for Rational
-- but Rational is a synonym for Ratio Integer,
-- which are rational
-- numbers using Integer for its fractions
-- and we have to jump through a hoop that makes this
-- more compositional to pacify the compiler
instance (Integral a,PerfectSqrt a) => PerfectSqrt (Ratio a) where
    perfectSqrt r = do
        n <- perfectSqrt (numerator r)
        d <- perfectSqrt (denominator r)
        return (n % d)

type Request = (Rational,Chan Response)
type Response = Maybe Interval

data Connection = Connection { service:: Chan Request, answers:: Chan Response }

-- Q13
setupSqrt :: Rational -> IO (Chan Request)
setupSqrt r = do
    c <- newChan
    forkIO (sqrtProcess r c)
    return c

-- Q14 
sqrtProcess :: Rational -> Chan Request -> IO()
sqrtProcess r c =
    do
        (marg, ans) <- readChan c
        if marg == 0
        then do
            -- x <- readChan c -- to clear the channel
            writeChan ans Nothing
        else do
            let result = sqrtAlgo r (mkInterval 0 r) marg
            if ilength result < 0
                then
                    writeChan ans Nothing
                else
                    writeChan ans (Just result)
        sqrtProcess r c

-- Q15                    
createConnection :: Chan Request -> IO Connection
createConnection req =
    do
        ans <- newChan
        return (Connection req ans)


-- Q16
useConnection :: Connection -> Rational -> IO (Maybe Interval)
useConnection c n =
    do
        writeChan (service c) (n, answers c)
        readChan (answers c)