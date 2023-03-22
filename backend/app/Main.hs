module Main where

import Network.Wai.Middleware.Static
import Options.Applicative
import Control.Monad
import Control.Exception
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Cors
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHC.Generics
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Network.Wai.Handler.Warp

type SymbolInformation = [Value]
type DiagnosticInformation = [Value]
type OpenFileInformation = [Value]

type SnippetRequestId = Int
type SnippetRequestMap = MVar (HM.HashMap SnippetRequestId (MVar Text))

data EditorState = EditorState {
    symbols :: SymbolInformation, -- vscode.Diagnostic[]
    diagnostics :: DiagnosticInformation, -- vscode.SymbolInformation[]
    openFiles :: OpenFileInformation, -- vscode.Uri[]
    currentFile :: Value -- vscode.Uri | null. null is used as a test of whether we're connected to the IDE
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ServerState = ServerState
  { editorStateVar :: MVar EditorState
  , snippetRequestMap :: SnippetRequestMap
  , websocketConnection :: MVar (Maybe Connection)
  , idCounter :: MVar Int
  }

data SnippetWebSocketRequest = SnippetWebSocketRequest {
    id :: SnippetRequestId,
    uri :: Value,
    range :: Value
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Args = Args {
  staticFileDir :: Maybe FilePath
} deriving Show

parseArgs :: Parser Args
parseArgs = Args
  <$> optional (strOption (long "static-file-dir" <> metavar "STATIC_FILE_DIR" <> help "Directory to serve static files from"))

queryWebsocketForSnippet :: ServerState -> SnippetWebSocketRequest -> IO ()
queryWebsocketForSnippet state request = do
    connection <- readMVar (websocketConnection state)
    case connection of
        Nothing -> return ()
        Just conn -> sendTextData conn (encode request)

data FulfillSnippetRequest = FulfillSnippetRequest {
    id :: SnippetRequestId,
    snippet :: Text
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

pushEditorState :: ServerState -> EditorState -> IO ()
pushEditorState state editorState = do
    modifyMVar_ (editorStateVar state) $ \_ -> return editorState

readEditorState :: ServerState -> IO EditorState
readEditorState state = do
    readMVar (editorStateVar state)

printEditorState :: ServerState -> IO ()
printEditorState state = do
    editorState <- readMVar (editorStateVar state)
    print editorState

data SnippetEndpointInput = SnippetEndpointInput {
    uri :: Value,
    range :: Value
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SnippetEndpointOutput = SnippetEndpointOutput {
    snippet :: Text
}
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

getSnippet :: ServerState -> Value -> Value -> IO Text
getSnippet state uri range = do
    snippetRequestId <- modifyMVar (idCounter state) $ \idCounter -> do
        return (idCounter + 1, idCounter)
    snippetRequestMVar <- newEmptyMVar
    modifyMVar_ (snippetRequestMap state) $ \snippetRequestMap -> do
        return (HM.insert snippetRequestId snippetRequestMVar snippetRequestMap)
    queryWebsocketForSnippet state (SnippetWebSocketRequest snippetRequestId uri range)
    takeMVar snippetRequestMVar

main :: IO ()
main = do
   args <- execParser (info (parseArgs <**> helper) fullDesc)
   -- Init state
   state <- ServerState <$> newMVar (EditorState [] [] [] Null) <*> newMVar HM.empty <*> newMVar Nothing <*> newMVar 0
   app <- Scotty.scottyApp $ mkApp args state
   let port = 4080
       settings = setPort port $ defaultSettings
       a = websocketsOr defaultConnectionOptions (wsApp state) app
   runSettings settings a  

isAlive :: Connection -> IO Bool
isAlive conn = do
    alive <- try $ sendPing conn ("" :: Text)
    case alive of
        Left (_ :: SomeException) -> return False
        Right _ -> return True

mkApp :: Args -> ServerState -> Scotty.ScottyM ()
mkApp args state = do
        -- Serve Static Files
        forM_ (staticFileDir args) $ \dir -> do
            Scotty.middleware $ staticPolicy (noDots >-> addBase dir)
        -- CORS to allow development from different ports
        Scotty.middleware $
             cors $ \_ ->
              Just
                simpleCorsResourcePolicy
                  { corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"],
                    corsOrigins = Just (["http://localhost:5173", "http://localhost:8080"], True),
                    corsRequestHeaders = ["authorization", "content-type"] }
 
        Scotty.get "/" $ do
            Scotty.html "<h1>Hello</h1>"

        Scotty.post "/appState" $ do
          syms <- Scotty.jsonData :: Scotty.ActionM EditorState
          liftIO $ pushEditorState state syms

        Scotty.get "/appState" $ do
          syms <- liftIO $ readEditorState state
          -- Change currentFile to null if there's no active connection
          connectionMaybe <- liftIO $ readMVar (websocketConnection state)
          syms' <- case connectionMaybe of
                Nothing -> return $ syms { currentFile = Null }
                Just connection -> do
                  -- check if connection is still alive
                  alive <- liftIO $ isAlive connection
                  if alive
                    then return syms
                    else do
                      liftIO $ modifyMVar_ (websocketConnection state) $ \_ -> return Nothing
                      return $ syms { currentFile = Null }
          Scotty.json syms'

        Scotty.post "/snippet" $ do
          snippetInput <- Scotty.jsonData :: Scotty.ActionM SnippetEndpointInput
          snippet <- liftIO $ getSnippet state snippetInput.uri snippetInput.range
          Scotty.json $ SnippetEndpointOutput snippet

        Scotty.post "/pingWebsocket" $ do
          connMay <- liftIO $ readMVar (websocketConnection state)
          case connMay of
              Nothing -> Scotty.text "No connection"
              Just conn -> do
                  liftIO $ sendTextData conn ("Ping" :: Text)
                  Scotty.text "Pinged"

        Scotty.get "/ping" $ do
          Scotty.text "pong"

        Scotty.post "/fulfillSnippet" $ do
          fulfillSnippetRequest <- Scotty.jsonData :: Scotty.ActionM FulfillSnippetRequest
          liftIO $ putStrLn $ "Fulfilling snippet request: " ++ show fulfillSnippetRequest
          liftIO $ modifyMVar_ (snippetRequestMap state) $ \snippetRequestMap -> do
            let snippetRequestId = fulfillSnippetRequest.id
            case HM.lookup snippetRequestId snippetRequestMap of
                Nothing -> return snippetRequestMap
                Just snippetRequestMVar -> do
                    putMVar snippetRequestMVar (fulfillSnippetRequest.snippet)
                    return (HM.delete snippetRequestId snippetRequestMap)

wsApp :: ServerState -> ServerApp
wsApp state pendingConn = do
  conn <- acceptRequest pendingConn
  modifyMVar_ (websocketConnection state) $ \_ -> return (Just conn)
  liftIO $ putStrLn "Connected!"
  Just conn' <- readMVar (websocketConnection state)
  withPingThread conn' 30 (return ()) $ forever $ do
    msg <- receiveData conn'
    liftIO $ putStrLn $ "Received: " ++ Text.unpack msg
