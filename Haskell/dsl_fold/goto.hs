-- http://d.hatena.ne.jp/fumiexcel/20121111/1352624678

import qualified Data.IntMap as M
import Control.Monad.Trans.Free
import Control.Monad.Trans

newtype Label = LabelId Int deriving (Eq, Ord)

data Labeling a = Label (Label -> a) | Goto Label

instance Functor Labeling where
    fmap f (Label g) = Label (f . g)
    fmap f (Goto l) = Goto l

type GotoT = FreeT Labeling

label :: Monad m => GotoT m Label
label = liftF (Label id)

goto :: Monad m => Label -> GotoT m ()
goto l = liftF (Goto l)

runGotoT :: Monad m => GotoT m a -> m a
runGotoT = run M.empty where
    run st m = runFreeT m >>= \r -> case r of
        Pure a -> return a
        Free (Label f) -> let cont = f (LabelId newLabel) in run (M.insert newLabel cont st) cont
        Free (Goto (LabelId i)) -> run st (st M.! i)
        where
            newLabel = succ (M.size st)

ex2 = do
    lift $ putStrLn "Begin."
    
    hoge <- label
    lift $ putStrLn "Label hoge."
    
    piyo <- label
    lift $ putStrLn "Label piyo."
    
    fuga <- label
    lift $ putStrLn "Label fuga."
        
    lift $ putStr "Where do you want to go? "
    str <- lift getLine
    case str of
        "hoge" -> goto hoge
        "piyo" -> goto piyo
        "fuga" -> goto fuga
        _ -> lift $ putStrLn "Quit."

main = runGotoT ex2
