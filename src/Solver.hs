module Solver where

import Data.List
import Numeric.LinearAlgebra
import System.IO
import FileIO(writeData, readColumn, readAllData)
import Graphic(drawGraphAll, drawGraphAllw, drawGraphAllq)
import System.Random
import StateAndObservation
import KalmanFilter



rungekutta4 :: [Double -> [Vector Double] -> Vector Double -> Matrix Double -> Vector Double]
            -> [Vector Double]
            -> [Vector Double]
            -> Matrix Double
            -> Double
            -> Double
            -> Double
            -> [Handle]
            -> [Handle]
            -> Handle
            -> Handle
            -> Bool
            -> [Vector Double]
            -> [Matrix Double]
            -> Vector Double
            -> Matrix Double
            -> Double
            -> IO ()
rungekutta4 ode@(ode_w:ode_q) xp@(w:q:_) xe@(we:qe:_) iner t dt limit handlew_list handle_estimate handle_error handle_vari use_kf means@(mmd:mob:_) vars@(qmd:rob:_) mp vp counter = do
    
    md <- calcMd mmd qmd 
    let mc = calcMc
        m_true  = mc + md
        m_est   = mc + vector [0,0,0]

    let state | ((length handlew_list) == 2) = [w, q]
              | otherwise                    = [vector [t], w, q]
        estimate_state = [we, qe]
        [vq0, vq1, vq2, vq3, vwx, vwy, vwz] = toList $ cmap sqrt $ takeDiag vp
        vari_state = [vector [vwx, vwy, vwx, vq0, vq1, vq2, vq3]]

    writeData state handlew_list
    if use_kf 
        then do
            writeData estimate_state handle_estimate
            writeData vari_state     [handle_vari]
        else return ()


    let xn    = rungekutta4OneStep ode xp t dt m_true iner
        xne'  = rungekutta4OneStep ode xe t dt m_est  iner
    
    if t > limit then return()
    else do
        (mn, vn, xne, new_counter) <- if use_kf 
                        then do
                            let (mn', vn') = predict mp vp xe iner dt vars 
                            -- if counter > (1/dt) 
                            if (t == 0) || (counter >= 0.1/dt)
                                then do
                                    (mn_, vn_) <- kfOneStep xp xe mn' vn' means vars
                                    let xne = removeError xne' mn_
                                        [dq0, dq1, dq2, dq3, dwx, dwy, dwz] = toList mn_
                                        dx_state = [vector [dwx, dwy, dwx, dq0, dq1, dq2, dq3]]
                                    writeData dx_state [handle_error]
                                    let (mn, vn) = ((konst 0 (size mn_) :: Vector R), vn_)
                                        new_counter = 0
                                    return (mn, vn, xne, new_counter)
                                else do
                                    let (mn, vn) = (mn', vn')
                                        [dq0, dq1, dq2, dq3, dwx, dwy, dwz] = toList mn
                                        dx_state = [vector [dwx, dwy, dwx, dq0, dq1, dq2, dq3]]
                                    writeData dx_state [handle_error]
                                    let xne = xne'
                                        new_counter = counter + 1
                                    return (mn, vn, xne, new_counter)
                        else return (mp, vp, xne', 0.0)

        rungekutta4 ode xn xne iner (t+dt) dt limit handlew_list handle_estimate handle_error handle_vari use_kf means vars mn vn new_counter

removeError xne'@(w:q:_) mn = xne
    where
        [dq0, dq1, dq2, dq3, dwx, dwy, dwz] = toList mn
        dq = vector [dq0, dq1, dq2, dq3]
        dw = vector [dwx, dwy, dwz]
        xne_ = [w+dw, q+dq]
        xne = normalize_q xne_




rungekutta4OneStep ode xs@(w:q:_) t dt m iner = xn
    where
            k1 = kf 0.0 0.0 $ [konst 0 $ size w :: Vector R, konst 0 $ size q :: Vector R]
            k2 = kf 0.5 0.5 k1
            k3 = kf 0.5 0.5 k2
            k4 = kf 1.0 1.0 k3
            x_dot  = integrate k1 k2 k3 k4 
            xn_raw = renew xs x_dot dt
            xn     = normalize_q xn_raw
            

            kf          tr  xr ki           = applyOde ode (t+tr*dt) (zipWith (\x k -> x+xr*scalar(dt)*k) xs ki) m iner
            applyOde    ode t  xs m  iner   = map (\f -> f t xs m iner) ode
            integrate   k1  k2 k3 k4        = zipWith4 (\l1 l2 l3 l4 -> (l1+2*l2+2*l3+l4)/6) k1 k2 k3 k4
            renew       xs  x_dots dt       = zipWith (\x xd-> x + xd*scalar(dt)) xs x_dots


ode_w :: Double -> [Vector Double] -> Vector Double -> Matrix Double -> Vector Double
ode_w t xs@(w:q:_) m iner = w_dot
        
        where
            l     = iner #> w
            wl    = cross w l
            dl    = wl - m
            w_dot = -dl / takeDiag iner

  
ode_q :: Double -> [Vector Double] -> Vector Double -> Matrix Double -> Vector Double
ode_q t xs@(w:q:_) m iner = q_dot

        where
            [q0, q1, q2, q3] = toList q
            qmatrix = fromLists [ [-q1, -q2, -q3]
                                 ,[ q0, -q3,  q2]
                                 ,[ q3,  q0, -q1]
                                 ,[-q2,  q1,  q0] ]
            q_dot = 0.5 * qmatrix #> w

calcMc = vector [0, 0, 0]

calcMd :: Vector Double -> Matrix Double -> IO (Vector Double)
calcMd mean_md qmd= do
    md <- createNoise mean_md qmd
    return md
    

normalize_q raw@(w_raw:q_raw:_) = w_raw : [q_raw / scalar(norm_2 q_raw)]
