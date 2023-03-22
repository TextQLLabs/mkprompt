module Main where

import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHC.Generics

type SymbolInformation = [Value]
type DiagnosticInformation = [Value]
type OpenFileInformation = [Value]

data EditorState = EditorState {
    symbols :: SymbolInformation,
    diagnostics :: DiagnosticInformation,
    openFiles :: OpenFileInformation
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ServerState = ServerState
  { editorStateVar :: MVar EditorState
  }

pushEditorState :: ServerState -> EditorState -> IO ()
pushEditorState state editorState = do
    modifyMVar_ (editorStateVar state) $ \_ -> return editorState

printEditorState :: ServerState -> IO ()
printEditorState state = do
    editorState <- readMVar (editorStateVar state)
    print editorState

main :: IO ()
main = do
    -- Init state
    state <- ServerState <$> newMVar (EditorState [] [] [])

    Scotty.scotty 4080 $ do
        Scotty.get "/" $ do
            Scotty.html "<h1>Hello</h1>"

        Scotty.post "/appState" $ do
          syms <- Scotty.jsonData :: Scotty.ActionM EditorState
          liftIO $ pushEditorState state syms
          liftIO $ printEditorState state
