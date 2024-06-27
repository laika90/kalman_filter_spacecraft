module Graphic where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

drawGraph :: FilePath -> [Double] -> [Double] -> String -> String -> String -> String -> IO ()
drawGraph filepath x y title xaxis yaxis legend = toFile def filepath $ do
    layout_title .= title 
    layout_x_axis . laxis_title .= xaxis
    layout_y_axis . laxis_title .= yaxis
    plot (line legend [zip x y])

drawGraphAll :: [FilePath] -> [Double] -> [[Double]] -> [String] -> String -> [String] -> [String] -> IO ()
drawGraphAll [] _ [] [] _ [] [] = return ()
drawGraphAll filepaths@(f:fs) t x_list@(x:xs) titles@(ti:tis) xaxis yaxis_list@(yaxis:yaxiss) legends@(le:les) = do
    drawGraph f t x ti xaxis yaxis le 
    drawGraphAll fs t xs tis xaxis yaxiss les 

drawGraphAllw :: FilePath -> [Double] -> [[Double]] -> String -> String -> String -> [String] -> IO ()
drawGraphAllw filepath t x@(wx:wy:wz:_) title xaxis yaxis legends@(lwx:lwy:lwz:_) = toFile def filepath $ do
    layout_title .= title
    layout_y_axis . laxis_title .= xaxis
    layout_y_axis . laxis_title .= yaxis
    plot (line lwx [zip t wx])
    plot (line lwy [zip t wy])
    plot (line lwz [zip t wz])

drawGraphAllq :: FilePath -> [Double] -> [[Double]] -> String -> String -> String -> [String] -> IO ()
drawGraphAllq filepath t x@(_:_:_:q0:q1:q2:q3:_) title xaxis yaxis legends@(_:_:_:lq0:lq1:lq2:lq3:_) = toFile def filepath $ do
    layout_title .= title
    layout_y_axis . laxis_title .= xaxis
    layout_y_axis . laxis_title .= yaxis
    plot (line lq0 [zip t q0])
    plot (line lq1 [zip t q1])
    plot (line lq2 [zip t q2])
    plot (line lq3 [zip t q3])

drawGraphAllCompare :: [FilePath] -> [Double] -> [[Double]] -> [[Double]] -> [String] -> String -> [String] -> [String] -> IO ()
drawGraphAllCompare [] _ [] [] [] _ _ _ = return ()
drawGraphAllCompare filepath@(f:fs) t x_true@(xt:xts) x_est@(xe:xes) title@(ti:tis) xaxis yaxis_list@(yaxis:yaxiss) legends = do
    drawGraphCompare f t xt xe ti xaxis yaxis legends
    drawGraphAllCompare fs t xts xes tis xaxis yaxiss legends

drawGraphCompare :: FilePath -> [Double] -> [Double] -> [Double] -> String -> String -> String -> [String] -> IO ()
drawGraphCompare filepath t x_true x_est title xaxis yaxis legends@(lxt:lxe:_) = toFile def filepath $ do
    layout_title .= title
    layout_y_axis . laxis_title .= xaxis
    layout_y_axis . laxis_title .= yaxis
    plot (line lxt [zip t x_true])
    plot (line lxe [zip t x_est])


    
