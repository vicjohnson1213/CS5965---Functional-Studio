import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hClose, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import System.Random
import Sudoku

main :: IO()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ generateSudoku handle
    sockHandler sock

generateSudoku :: Handle -> IO()
generateSudoku handle = do
    hGetLine handle
    gen <- newStdGen
    let b = generate gen
    hPutStrLn handle (printUnsolved b)
    hClose handle

