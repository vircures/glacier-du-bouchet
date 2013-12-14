module Handler.Fib where

import Import
import Control.Concurrent (forkIO)
--import Control.Monad(liftM)
import Text.Printf (printf)

fib :: Int -> Int
fib = 
    let fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n - 1) + fib (n - 2)
    in  (map fib' [0..] !!)

getFibR :: Int -> Handler Html
getFibR n = 
    defaultLayout $ do
            let fibres = fib n
            [whamlet|<p>Fib for #{n} is #{show fibres}!|]

{-
fibIO :: Int -> IO()
fibIO n = do
    let fibres = fib n
    printf "fib %d is %d\n" n fibres
    return $ show fibres

getFibR :: Int -> Handler Html
getFibR n = do
    -- Lookup the name value set in the session
    tid <- lift $ forkIO $ fibIO n
    defaultLayout [whamlet|<p>Fib for #{n} is calculated in thread #{show tid}...|]
-}
