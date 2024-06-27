module KalmanFilter where

import Prelude hiding ((<>))
import Data.List
import Numeric.LinearAlgebra
import StateAndObservation

predict :: Vector Double -> Matrix Double -> [Vector Double] -> Matrix Double -> Double -> [Matrix Double] -> (Vector Double, Matrix Double) 
predict mp vp xs iner dt vars@(q:r:_) = (mn', vn')
    where
        (a, a') = calcA xs iner dt
        b = calcB iner dt a a'
        mn' = a #> mp
        vn' = a <> vp <> (tr a) + b <> q <> (tr b)


kfOneStep :: [Vector Double] -> [Vector Double] -> Vector Double -> Matrix Double -> [Vector Double] -> [Matrix Double] -> IO (Vector Double, Matrix Double)
kfOneStep xp xe mn' vn' means vars@(q:r:_) =  do
    (col_index, zn) <- observeAtRandom xp xe means vars
    let h = calcH xe col_index
        kn = vn' <> (tr h) <> (inv (h <> vn' <> (tr h) + r))
        mn = mn' + kn #> (zn - h #> mn')
        (row, col) = size vn'
        vn = (ident row - kn <> h) <> vn'
    return (mn, vn)

    
    
