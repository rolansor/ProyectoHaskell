import System.Random 
import Data.List  

--The following Emurated type hold all the colors to be used 
data Colores = V | C | B | M | A | R deriving (Eq, Ord, Show)
--The following function filters all the possible combination for the game
combinaciones = [ [a, b, c ,d] | a <- rs, b <- rs, c <- rs, d <- rs] where rs = [V, C, B, M, A, R]

--The following functions states all the possible moves a user can make
movimientos = putStrLn$ "Existen " ++ (show$ length combinaciones) ++ " posibles movimientos"




miRandom3 =  do 
     newStdGen
     g<-getStdGen
     let 
        a = 0::Int
        b = 1000::Int
        x = fst $ randomR (a, b) g
     return  x
	 

	 
f 1 sum = return sum
f n sum = do
  x  <- miRandom3
  xs <- f (n - 1) (sum - x)
  a <- combinaciones 
  return xs