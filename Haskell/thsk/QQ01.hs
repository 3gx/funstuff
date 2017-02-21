{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module QQ01 (module QQ01) where

import qualified Data.Text as T
import Data.Text (Text,unpack)
import Data.Attoparsec.Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

string :: QuasiQuoter
string = QuasiQuoter
    { quoteExp = \s -> [| s |],
      -- OR, with Language.Haskell.TH.Syntax imported: quoteExp = lift
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }



type Chunk = Either Text String
-- a chunk is either just raw Text, or the String name of a variable

makeChunks :: Text -> [Chunk]
makeChunks ts = case parseOnly parser ts of
    Right x -> x
    _       -> error "malformed text"
  where
    parser = do
        res <- loop []
        return [ rs | 
                   rs <- res, 
                   rs /= Left  "", 
                   rs /= Right ""]

    variable = do
        char '$'
        name <- takeTill (notInClass "a-zA-Z0-9_")
        return (Right (T.unpack name))

    loop xs = do
        text <- takeTill (== '$')
        var  <- choice [variable, fmap Left takeText]
        end  <- atEnd
        if end
            then return $ reverse (var : Left text : xs)
            else loop (var : Left text : xs)

instance Lift Text where
   lift t = litE (stringL (unpack t))

format :: QuasiQuoter
format = QuasiQuoter
    { quoteExp = \s ->
        let chunks       = makeChunks (T.pack s)
            liftedChunks = flip map chunks $ \c -> case c of
                Left  t -> [| t |]           -- lift raw text
                Right v -> varE (mkName v) -- get a global Name from the name given


        -- and now to fold it all together ... 
        in foldr (\l r -> appE [| T.append |] l `appE` r) [| T.empty |] liftedChunks,

        -- note that: appE :: Q Exp -> Q Exp -> Q Exp; it acts as function application for Q Exps
        --            [| T.append |] is the Q Exp form of T.append
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

format1 :: QuasiQuoter
format1 = QuasiQuoter
    { quoteExp = \s ->
        let chunks       = makeChunks (T.pack s)
            liftedChunks = flip map chunks $ \c -> case c of
                Left  t -> [| t |]           -- lift raw text
                Right v -> varE (mkName v) -- get a global Name from the name given
        in appE [| T.concat |] (listE liftedChunks),
        -- note that listE :: [Q Exp] -> Q Exp
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

