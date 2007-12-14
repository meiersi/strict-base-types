import qualified Prelude as Lazy (readFile)
import Prelude (putStrLn, putChar)
import System.IO.Strict
import Control.Monad

main = do
    let n = [0..99999]
    putStrLn "Strict IO"
    mapM_ strict n
    putChar '\n'

    putStrLn "Lazy IO"
    mapM_ lazy   n -- should fail
    putChar '\n'

 where
    strict i = do      readFile "files.hs"; putChar '!'
    lazy   i = do Lazy.readFile "files.hs"; putChar '~'
