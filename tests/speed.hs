
import Text.Printf
import Control.Exception
import System.CPUTime

import qualified Data.ByteString as S

import Prelude hiding (readFile)
import qualified Prelude as Lazy (readFile)
import System.IO.Strict
import Control.Monad

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    let n = [0..500]
    putStrLn "Strict IO"
    time $ mapM_ strict n
    putChar '\n'

    putStrLn "Strict ByteString IO"
    time $ mapM_ bytestring n
    putChar '\n'

 where
    strict       i = do   readFile "/usr/share/dict/words"; putChar '!'
    bytestring   i = do S.readFile "/usr/share/dict/words"; putChar '~'
