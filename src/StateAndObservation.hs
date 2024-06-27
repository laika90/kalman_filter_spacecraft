module StateAndObservation where

import Prelude hiding ((<>))
import Data.List
import Numeric.LinearAlgebra
import System.IO
import FileIO(writeData, readColumn, readAllData)
import Graphic(drawGraphAll, drawGraphAllw, drawGraphAllq)
import System.Random


calcA :: [Vector Double] -> Matrix Double -> Double -> (Matrix Double, Matrix Double)
calcA xs@(w:q:_) iner dt = (a, a')
    where
        [wx, wy, wz] = toList w
        [q0, q1, q2, q3] = toList q
        [ix, iy, iz] = toList $ takeDiag iner

        a' = fromLists [ [      0, -0.5*wx, -0.5*wy, -0.5*wz,       -0.5*q1,       -0.5*q2,       -0.5*q3 ]
                        ,[ 0.5*wx,       0,  0.5*wz, -0.5*wy,        0.5*q0,       -0.5*q3,        0.5*q2 ]  
                        ,[ 0.5*wy, -0.5*wz,       0,  0.5*wx,        0.5*q3,        0.5*q0,       -0.5*q1 ]
                        ,[ 0.5*wz,  0.5*wy, -0.5*wx,       0,       -0.5*q2,        0.5*q1,        0.5*q0 ]
                        ,[      0,       0,       0,       0,             0, (iy-iz)/ix*wz, (iy-iz)/ix*wy ]
                        ,[      0,       0,       0,       0, (iz-ix)/iy*wz,             0, (iz-ix)/iy*wx ]
                        ,[      0,       0,       0,       0, (ix-iy)/iz*wy, (ix-iy)/iz*wx,             0 ] ]
        (row, col) = size a'
        a = expm (a'*scalar(dt))
        
calcB :: Matrix Double -> Double -> Matrix Double -> Matrix Double -> Matrix Double
calcB iner dt a a' = b
    where
        [ix, iy, iz] = toList $ takeDiag iner

        b' = fromLists [ [   0,    0,    0] 
                        ,[   0,    0,    0] 
                        ,[   0,    0,    0]
                        ,[   0,    0,    0] 
                        ,[1/ix,    0,    0] 
                        ,[   0, 1/iy,    0] 
                        ,[   0,    0, 1/iz] ]
        (row, col) = size a
        -- b = inv a' <> (a - ident row) <> b'
        b = b' * scalar(dt)
        

calcH :: [Vector Double] -> Int -> Matrix Double
calcH xs@(w:q:_) col_index = h
    where
        [q0, q1, q2, q3] = toList q
        tmp = if col_index == 0
                then fromLists [ [ q0, q1, -q2, -q3, 0, 0, 0]
                                ,[ q3, q2,  q1,  q0, 0, 0, 0] 
                                ,[-q2, q3, -q0,  q1, 0, 0, 0] ]
                else if col_index == 1
                        then fromLists [ [-q3,  q2,  q1, -q0, 0, 0, 0]
                                        ,[ q0, -q1,  q2, -q3, 0, 0, 0] 
                                        ,[ q1,  q0,  q3,  q2, 0, 0, 0] ]
                        else fromLists [ [ q2,  q3,  q0,  q1, 0, 0, 0]
                                        ,[-q1, -q0,  q3,  q2, 0, 0, 0] 
                                        ,[ q0, -q1, -q2,  q3, 0, 0, 0] ]
        h = 2.0 * tmp

extractColumn :: [Vector Double] -> Int -> Vector Double
extractColumn xs@(w:q:_) col_index = column
    where
        [q0, q1, q2, q3] = toList q
        column = if col_index == 0
                    then fromList [ q0^2+q1^2-q2^2-q3^2,
                                    2*(q1*q2 + q0*q3),  
                                    2*(q1*q3 - q0*q2)   ]
                    else if col_index == 1
                            then fromList [ 2*(q1*q2 - q0*q3),
                                            q0^2-q1^2+q2^2-q3^2,
                                            2*(q2*q3 + q0*q1)   ]
                            else fromList [ 2*(q1*q3 + q0*q2),
                                            2*(q2*q3 - q0*q1),
                                            q0^2-q1^2-q2^2+q3^2 ]

observeAtRandom :: [Vector Double] -> [Vector Double] -> [Vector Double]-> [Matrix Double] -> IO (Int, Vector Double)
observeAtRandom xp@(w:q:_) xe@(we:qe:_) means@(_:mob:_) vars@(_:rob:_)= do
    rand_num <- randomIO :: IO Double
    let col_index | rand_num < (1.0/3.0) = 0
                  | rand_num < (2.0/3.0) = 1
                  | otherwise            = 2
        column_true     = extractColumn xp col_index
        column_estimate = extractColumn xe col_index
        column_error = column_true - column_estimate
    noise <- createNoise mob rob
    return (col_index, column_error+noise)


createNoise :: Vector Double -> Matrix Double -> IO (Vector Double)
createNoise mean var = do
    let sigma = toList $ cmap sqrt (takeDiag var) 
    noise_x <- randomRIO (-1, 1) :: IO Double
    noise_y <- randomRIO (-1, 1) :: IO Double
    noise_z <- randomRIO (-1, 1) :: IO Double
    let noise = vector $ zipWith3 (\ni mi si -> ni*si + mi) [noise_x, noise_y, noise_z] (toList mean) sigma
    return noise

generateInitialState :: Int -> IO [Vector Double]
generateInitialState seed = do
        let gen = mkStdGen seed
        wx <- randomRIO (-1, 1) :: IO Double
        wy <- randomRIO (-1, 1) :: IO Double
        wz <- randomRIO (-1, 1) :: IO Double
        q0 <- randomRIO (-1, 1) :: IO Double
        q1 <- randomRIO (-1, 1) :: IO Double
        q2 <- randomRIO (-1, 1) :: IO Double
        q3 <- randomRIO (-1, 1) :: IO Double
        let w = vector [wx, wy, wz]
            q = vector [q0, q1, q2, q3]
            xe = [w, q / scalar(norm_2 q)]
        return xe

