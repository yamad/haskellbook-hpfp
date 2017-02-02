{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

import Control.Monad.IO.Class

data Config = Config
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)


-- Got stuck here, so some critical parts are cribbed from
-- github.com/dmvianna/haskellbook. Most importantly, the definition
-- of `runR` and the retreival of environment/state via `ask` let me
-- get to the rest of answer for myself. But dmvianna's answer also
-- has some nice stylistic features that I ended up refactoring my
-- code into, as I figure out good Haskell style.

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k new m, new)
  where new = fromMaybe 0 (M.lookup k m) + 1

app :: Scotty ()
app =
  get "/:key" $
  do config <- lift ask         -- retrieve environment from reader
     unprefixed <- param "key"
     let key' = mappend (prefix config) unprefixed
         ior  = counts config                          -- get IORef Map value
         cmap = readIORef ior
     (cmap', newI) <- liftIO $ bumpBoomp key' <$> cmap -- calc new values
     liftIO $ writeIORef ior cmap'                     -- save value to IORef
     html $
       mconcat ["<h1>Success! Count was: ", TL.pack $ show newI, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty                     -- make mutable map
  let config = Config counter $ TL.pack prefixArg -- initialize config/state
      runR r = runReaderT r $ config              -- reader carries in
                                                  -- config as environment
  scottyT 3000 runR app
