import Data.List
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import System.IO
import FileIO(writeData, readColumn, readAllData, readAllErrorAndVariance)
import Graphic(drawGraphAll, drawGraphAllw, drawGraphAllq, drawGraphAllCompare)
import System.Random
import StateAndObservation
import Solver

 
main :: IO ()
main = do

    setStdGen (mkStdGen seed)

    -- 各ハンドラ (write)
    handle_t <- openFile path_to_t WriteMode
    handle_w <- openFile path_to_raw_w WriteMode
    handle_q <- openFile path_to_raw_q WriteMode
    let handle_raw = [handle_t, handle_w, handle_q]

    handle_true_w <- openFile path_to_true_w WriteMode
    handle_true_q <- openFile path_to_true_q WriteMode
    let handle_true = [handle_true_w, handle_true_q]

    handle_est_w <- openFile path_to_est_w WriteMode
    handle_est_q <- openFile path_to_est_q WriteMode
    let handle_estimate = [handle_est_w, handle_est_q]

    handle_error <- openFile path_to_error WriteMode
    handle_vari  <- openFile path_to_vari WriteMode

    xe0 <- generateInitialState seed
    print xe0

    --          ode xp xe  iner t dt   limit handle      handle_estimate handle_error handle_vari use_kf means  vars  mp  vp  counter
    rungekutta4 ode x0 xe0 iner 0 0.01 100   handle_raw handle_estimate handle_error handle_vari False  means0 vars01 mp0 vp0 0  

    mapM_ hClose handle_raw

    let path_list = [path_to_t, path_to_raw_w, path_to_raw_q]
    all_data <- readAllData path_list 

    case all_data of
        [] -> do
            putStrLn "No data available"
        (t_list : x_list) -> do
            let graph_path = [path_to_wx, path_to_wy, path_to_wz, path_to_q0, path_to_q1, path_to_q2, path_to_q3]
                titles = ["wx", "wy", "wz", "q0", "q1", "q2", "q3"]
                xaxis = "t [s]"
                yaxis = ["wx [rad/s]", "wy [rad/s]", "wz [rad/s]", "q0", "q1", "q2", "q3"]
                legends = ["wx", "wy", "wz", "q0", "q1", "q2", "q3"]
            
            drawGraphAll  graph_path   t_list x_list titles  xaxis   yaxis       legends
            drawGraphAllw path_to_wall t_list x_list "w all" "t [s]" "w [rad/s]" legends
            drawGraphAllq path_to_qall t_list x_list "q all" "t [s]" "q"         legends

  

    --          ode xp xe  iner t dt   limit handle      handle_estimate handle_error handle_vari use_kf means  vars  mp  vp  counter
    rungekutta4 ode x0 xe0 iner 0 0.01 100   handle_true handle_estimate handle_error handle_vari True  means0 vars02 mp0 vp0 0  

    mapM_ hClose handle_estimate
    mapM_ hClose handle_true
    hClose handle_error
    hClose handle_vari

    let path_estimate = [path_to_t, path_to_est_w,  path_to_est_q]
        path_true     = [path_to_t, path_to_true_w, path_to_true_q]
    estimate_data <- readAllData path_estimate
    true_data     <- readAllData path_true

    case estimate_data of
        [] -> do
            putStrLn "No data available"
        (t_est : x_est) -> do
            case true_data of
                [] -> do
                    putStrLn "No data available"
                (t_true : x_true) -> do
                    let graph_true_vs_estimate = [path_to_true_vs_est_wx,
                                                  path_to_true_vs_est_wy, 
                                                  path_to_true_vs_est_wz,
                                                  path_to_true_vs_est_q0, 
                                                  path_to_true_vs_est_q1,
                                                  path_to_true_vs_est_q2,
                                                  path_to_true_vs_est_q3]
                        titles_true_vs_estimate = ["true vs estimate (wx)",
                                                   "true vs estimate (wy)",
                                                   "true vs estimate (wz)",
                                                   "true vs estimate (q0)",
                                                   "true vs estimate (q1)",
                                                   "true vs estimate (q2)",
                                                   "true vs estimate (q3)" ]

                        xaxis_true_vs_estimate = "t [s]"
                        yaxis_true_vs_estimate = ["wx [rad/s]", "wy [rad/s]", "wz [rad/s]", "q0", "q1", "q2", "q3"]
                        legends_true_vs_estimate = ["true", "estimate"]
            
                    drawGraphAllCompare  graph_true_vs_estimate t_true x_true x_est titles_true_vs_estimate  xaxis_true_vs_estimate   yaxis_true_vs_estimate       legends_true_vs_estimate

    let path_error_list = [path_to_t, path_to_error]
        path_vari_list  = [path_to_t, path_to_vari]
    error_data <- readAllErrorAndVariance path_error_list
    vari_data  <- readAllErrorAndVariance path_vari_list

    case error_data of
        [] -> do
            putStrLn "No data available"
        (t_error:x_error) -> do
            case vari_data of
                [] -> do
                    putStrLn "No data available"
                (t_vari:x_vari) -> do
                    let graph_error_vs_vari = [path_to_error_vs_vari1,
                                               path_to_error_vs_vari2,
                                               path_to_error_vs_vari3,
                                               path_to_error_vs_vari4,
                                               path_to_error_vs_vari5,
                                               path_to_error_vs_vari6,
                                               path_to_error_vs_vari7]
                        titles_error_vs_vari = ["error vs standard diviation (wx)",
                                              　"error vs standard diviation (wy)",
                                                "error vs standard diviation (wz)",
                                                "error vs standard diviation (q0)",
                                                "error vs standard diviation (q1)",
                                                "error vs standard diviation (q2)",
                                                "error vs standard diviation (q3)" ]

                        xaxis_error_vs_vari = "t [s]"
                        yaxis_error_vs_vari = ["wx [rad/s]", "wy [rad/s]", "wz [rad/s]", "q0", "q1", "q2", "q3"]
                        legends_error_vs_vari = ["error", "standard diviation"]
            
                    drawGraphAllCompare  graph_error_vs_vari t_error x_error x_vari titles_error_vs_vari xaxis_error_vs_vari yaxis_error_vs_vari legends_error_vs_vari

    return ()

    where
        seed = 42
        w0   = vector [0.1, 17*2*pi/60 + 0.1, 0]
        q0   = vector [1, 0, 0, 0]
        x0   = [w0, q0]
        -- xe0  = [vector [1, 1, 1], vector [0.5, 0.5, 0.5, 0.5]]
        ode  = [ode_w, ode_q]
        iner = fromLists [ [1.9,   0,   0]
                          ,[  0, 1.6,   0]
                          ,[  0,   0, 2.0] ]
        means0 = [0, 0]
        qmatrix01 :: Matrix Double = fromLists [ [0, 0, 0]
                                                ,[0, 0, 0]
                                                ,[0, 0, 0] ]
        rmatrix01 :: Matrix Double = fromLists [ [0, 0, 0]
                                                ,[0, 0, 0]
                                                ,[0, 0, 0] ]
        vars01 = [qmatrix01, rmatrix01]

        qmatrix02 :: Matrix Double = fromLists [ [0.0001,    0,    0]
                                                ,[   0, 0.0001,    0]
                                                ,[   0,    0, 0.0001] ]
        rmatrix02 ::Matrix Double = fromLists [ [0.0001,    0,    0]
                                               ,[   0, 0.0001,    0]
                                               ,[   0,    0, 0.0001] ]
        vars02 = [qmatrix02, rmatrix02]

        mp0 = vector [0, 0, 0, 0, 0, 0, 0]
        vp0 = ident 7  

        

        -- 今回は制御・外乱トルクは各ステップの中で定義

        -- 保存先ファイル
        path_to_t = "../result/t.dat"
        path_to_raw_w  = "../result/raw/raw_w.dat"
        path_to_raw_q  = "../result/raw/raw_q.dat"
        path_to_true_w = "../result/true/true_w.dat"
        path_to_true_q = "../result/true/true_q.dat"
        path_to_est_w  = "../result/estimate/est_w.dat"
        path_to_est_q  = "../result/estimate/est_q.dat"
        path_to_error = "../result/error/error.dat"
        path_to_vari = "../result/error/sqrt_vari.dat"

        -- グラフ保存先
        path_to_wx   = "../graph/raw/wx.svg"
        path_to_wy   = "../graph/raw/wy.svg"
        path_to_wz   = "../graph/raw/wz.svg"
        path_to_q0   = "../graph/raw/q0.svg"
        path_to_q1   = "../graph/raw/q1.svg"
        path_to_q2   = "../graph/raw/q2.svg"
        path_to_q3   = "../graph/raw/q3.svg"
        path_to_wall = "../graph/raw/wall.svg"
        path_to_qall = "../graph/raw/qall.svg"
        path_to_true_vs_est_wx = "../graph/compare/wx.svg"
        path_to_true_vs_est_wy = "../graph/compare/wy.svg"
        path_to_true_vs_est_wz = "../graph/compare/wz.svg"
        path_to_true_vs_est_q0 = "../graph/compare/q0.svg"
        path_to_true_vs_est_q1 = "../graph/compare/q1.svg"
        path_to_true_vs_est_q2 = "../graph/compare/q2.svg"
        path_to_true_vs_est_q3 = "../graph/compare/q3.svg"
        path_to_error_vs_vari1 = "../graph/compare/11.svg"
        path_to_error_vs_vari2 = "../graph/compare/22.svg"
        path_to_error_vs_vari3 = "../graph/compare/33.svg"
        path_to_error_vs_vari4 = "../graph/compare/44.svg"
        path_to_error_vs_vari5 = "../graph/compare/55.svg"
        path_to_error_vs_vari6 = "../graph/compare/66.svg"
        path_to_error_vs_vari7 = "../graph/compare/77.svg"
    



    
