module Main(main) where

import           Graphics.Vty
import           Graphics.Vty.Prelude (regionHeight)
import           SimpleGetOpt

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.List(isInfixOf, isPrefixOf)
import           Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import           System.FilePath
import           Control.Concurrent (forkIO)
import           Control.Monad (unless)
import           Control.Exception(handle,catch,SomeException(..))
import           System.Process(runInteractiveProcess,terminateProcess)
import           System.IO(hGetContents,hSetBuffering,BufferMode(LineBuffering)
                          , hPutStrLn, stderr )
import           System.Directory
                      (getHomeDirectory,doesFileExist,doesDirectoryExist)
import           System.Exit(exitFailure)
import           System.Environment(getArgs)
import           Data.Char(isSpace)
import           Text.Read (readMaybe)
import           Data.Bits (testBit)


type Name   = String
data State  = State { buffers    :: !(Map FilePath (Status,Buffer))
                    , pageHeight :: !Int
                    , watching   :: !(Maybe Name)
                    , prevBufs   :: ![Name]
                    , curBuf     :: !(Maybe Name)
                    , nextBufs   :: ![Name]
                    , logDir     :: FilePath
                    } deriving Show

data Status    = Modified | Unmodified
                 deriving Show

data Settings = Settings
  { fsOpts      :: [String]
  , fsPath      :: FilePath
  , fsWatchThis :: Maybe FilePath
  }

options :: OptSpec Settings
options = OptSpec
  { progDefaults = Settings { fsOpts      = []
                            , fsPath      = "fswatch"
                            , fsWatchThis = Nothing }

  , progOptions =
      [ Option ['m'] ["monitor"]
        "Use this monitor (see: fswatch -m)."
        $ ReqArg "MONITOR" $ \a s -> Right s { fsOpts = "-m" : a : fsOpts s }

      , Option ['l'] ["latency"]
        "Use the latency (see: fswatch -l)."
        $ ReqArg "DOUBLE" $ \a s ->
          case readMaybe a of
            Just n | n > 0.1  -> Right s { fsOpts = "-l" : a : fsOpts s }
            _                 -> Left "Invalid latency."

      , Option ['p'] ["path"]
        "The path for the `fswatch` executable."
        $ ReqArg "PATH" $ \a s -> Right s { fsPath = a }

      , Option ['d'] ["dir"]
        "Watch this directory of logs."
        $ ReqArg "DIR" $ \a s -> Right s { fsWatchThis = Just a }
      ]

  , progParamDocs = []
  , progParams = \p _ -> Left ("Unknown parameter: " ++ p)
  }

main :: IO ()
main =
  do as <- getOpts options
     d  <- case fsWatchThis as of
             Nothing -> guessLogDir
                `catch` \SomeException {} ->
                            do hPutStrLn stderr "Failed to work out directory"
                               exitFailure
             Just d -> return d

     (_,hOut,_,p) <- runInteractiveProcess
        (fsPath as) ("-r" : "-x" : "-n" : fsOpts as ++ [d]) Nothing Nothing
        `catch` \SomeException {} ->
                    do hPutStrLn stderr ("Failed to start " ++ show (fsPath as))
                       exitFailure
     hSetBuffering hOut LineBuffering
     txt <- hGetContents hOut

     -- Start VTY
     cfg  <- standardIOConfig
     vty  <- mkVty cfg
     bnds <- displayBounds (outputIface vty)
     st   <- newIORef State { buffers    = Map.empty
                            , pageHeight = regionHeight bnds
                            , prevBufs   = []
                            , curBuf     = Nothing
                            , nextBufs   = []
                            , watching   = Nothing
                            , logDir     = d
                            }

     _ <- forkIO $ mapM_ (\x -> do handleUpdate st x
                                   redraw vty st)
                 $ mapMaybe getUpdate $ lines txt
     vtyLoop vty st
     terminateProcess p

guessLogDir :: IO FilePath
guessLogDir =
  do cfg <- configFile
     txt <- readFile cfg
     case mapMaybe isLogLine (lines txt) of
       l : _ -> return l
       _ -> do d <- getHomeDirectory
               return (d </> ".cabal" </> "logs")
  where
  configFile = do let sandbox = "cabal.sandbox.config"
                  yes <- doesFileExist sandbox
                  if yes
                    then return sandbox
                    else do d <- getHomeDirectory
                            return (d </> ".cabal" </> "config")
  isLogLine x =
    case break (== ':') x of
      (as,_:bs) | trim as == "logs-dir" -> Just (trim bs)
      _                                 -> Nothing

  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


vtyLoop :: Vty -> IORef State -> IO ()
vtyLoop vty ref =
  do redraw vty ref
     evt <- nextEvent vty
     case evt of
       EvResize _ y -> upd $ \st -> st { pageHeight = y }
       EvMouse {}   -> again
       EvKey k _ ->
         case k of
           KEsc -> shutdown vty

           KChar 'r' ->
             do st <- readIORef ref
                case curBuf st of
                  Just b -> do handleUpdate ref b
                               redraw vty ref
                               again
                  Nothing -> again


           KEnter ->
            upd $ \st ->
              case curBuf st of
                Nothing -> st
                Just x  ->
                  let unmod (_,y) = (Unmodified,y)
                  in st { watching = Just x
                        , buffers = case watching st of
                                      Just y  -> Map.adjust unmod y (buffers st)
                                      Nothing -> buffers st
                        }

           KUp ->
             upd $ \st ->
               case prevBufs st of
                 [] -> st
                 x : xs -> st { prevBufs = xs
                              , curBuf = Just x
                              , nextBufs =
                                  case curBuf st of
                                    Nothing -> nextBufs st
                                    Just a -> a : nextBufs st }

           KDown ->
             upd $ \st ->
               case nextBufs st of
                 [] -> st
                 x : xs -> st { nextBufs = xs
                              , curBuf = Just x
                              , prevBufs =
                                  case curBuf st of
                                    Nothing -> prevBufs st
                                    Just a -> a : prevBufs st }

           KPageUp   -> updWatch bufPageUp
           KPageDown -> updWatch bufPageDown
           KLeft     -> updWatch (\_ -> bufLeft)
           KRight    -> updWatch (\_ -> bufRight)


           _          -> again
  where
  again = vtyLoop vty ref
  upd f = do modifyIORef' ref f
             again

  updWatch f =
    upd $ \st ->
      case watching st of
        Nothing -> st
        Just b  -> st { buffers = Map.adjust f' b (buffers st) }
          where f' (x,y) = (x,f (pageHeight st) y)


redraw :: Vty -> IORef State -> IO ()
redraw vty ref =
  do st <- readIORef ref
     update vty $ picForImage $ draw st

draw :: State -> Image
draw st = heading <-> (files <|> preview)
  where
  heading = string (attr black yellow) (logDir st)

  files = vertCat $ map (ppItem False) (reverse (prevBufs st)) ++
                    [ ppItem True (fromMaybe "(no files)" (curBuf st)) ] ++
                    map (ppItem False) (nextBufs st)

  ppItem sel x = string a name -- (name ++ " (" ++ show n ++ ")")
    where
    name    = dropExtension (takeFileName x)
    a       = if sel then a' `withStyle` reverseVideo else a'
    (a',n)  = case Map.lookup x (buffers st) of
                 Nothing -> (noramlText, 0)
                 Just (sta,b) ->
                   case sta of
                     _ | Just x == watching st -> (watchingTxt, chLineNum b)
                     Modified                  -> (changedText, chLineNum b)
                     Unmodified                -> (noramlText,  chLineNum b)

  preview = case (`Map.lookup` buffers st) =<< watching st of
              Just (_,buf) -> vertCat $ map ppLine
                                      $ bufVisiable (pageHeight st) buf
              _            -> string noramlText "(not watching)"

  attr fg bg = withForeColor (withBackColor defAttr bg) fg

  noramlText  = attr white       black
  changedText = attr red         black
  watchingTxt = attr green       black
  brightTxt   = attr brightGreen black

  ppLine (x,i) = string (if i then brightTxt else noramlText) x

--------------------------------------------------------------------------------

updateBit :: Int
updateBit = 2

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n xs = take (length xs - n) xs

getUpdate :: String -> Maybe FilePath
getUpdate txt =
  case words txt of
    xs@(_ : _)
      | let numStr = last xs
      , Just n <- readMaybe numStr
      , testBit (n::Int) updateBit -> Just (dropFromEnd (length numStr + 1) txt)
    _ -> Nothing

handleUpdate :: IORef State -> FilePath -> IO ()
handleUpdate ref file =
  handle (\SomeException {} -> return ()) $
  do isDir <- doesDirectoryExist file
     unless isDir $
       do txt <- readFile file
          let ls     = lines txt
              lNum   = length ls
          seq lNum $ modifyIORef' ref $ \st ->
            case Map.lookup file (buffers st) of

               -- Is this a new buffer?
               Nothing ->
                 let bufs1 = Map.insert file
                            (Modified, Buffer { chText    = ls
                                              , chStart   = Nothing
                                              , chLineNum = lNum
                                              , chLineSt  = Nothing
                                              })
                            (buffers st)
                 in case curBuf st of
                      Nothing -> st { buffers  = bufs1
                                    , curBuf   = Just file
                                    , watching = Just file
                                    }
                      Just _  -> st { buffers  = bufs1
                                    , nextBufs = file : nextBufs st
                                    }

               -- Existing buffer
               Just (_,b)  -> st { buffers = Map.insert file
                                               (Modified, bufSetText
                                                           (pageHeight st) ls b)
                                               (buffers st) }



--------------------------------------------------------------------------------

data Buffer  = Buffer { chText       :: ![String]
                      , chLineNum    :: !Int
                      , chStart      :: !(Maybe Int)
                      , chLineSt     :: !(Maybe Int)
                      } deriving Show

interestingLine :: String -> Bool
interestingLine x = "Linking" `isPrefixOf` x
                 || "Registering" `isPrefixOf` x
                 || "registering" `isInfixOf` x

bufVisiable :: Int -> Buffer -> [(String,Bool)]
bufVisiable h c =
  case chLineSt c of
    Nothing -> ls1
    Just x  -> [ (drop x cs, i) | (cs,i) <- ls1 ]
  where
  ls1 = [ (l, interestingLine l) | l <- ls ]
  ls  = case chStart c of
          Just n  -> take h $ drop n $ chText c
          Nothing -> drop (chLineNum c - h) (chText c)

bufRight :: Buffer -> Buffer
bufRight c = c { chLineSt = case chLineSt c of
                              Nothing -> Just 2
                              Just n  -> Just (n + 2) }

bufLeft :: Buffer -> Buffer
bufLeft c = c { chLineSt = case chLineSt c of
                             Just n | n > 2 -> Just (n - 2)
                             _              -> Nothing }

bufPageUp :: Int -> Buffer -> Buffer
bufPageUp h c = c { chStart = Just (max 0 (start - pageStep h)) }
  where start = case chStart c of
                  Just n  -> n
                  Nothing -> max 0 (chLineNum c - h)

bufPageDown :: Int -> Buffer -> Buffer
bufPageDown h c =
  case chStart c of
    Nothing -> c
    Just n -> bufCorrectEndOfText h c { chStart = Just (n + pageStep h) }

pageStep :: Int -> Int
pageStep h = max 1 (div (2 * h) 3)

bufCorrectEndOfText :: Int -> Buffer -> Buffer
bufCorrectEndOfText h c =
  case chStart c of
    Just n | n >= chLineNum c - h -> c { chStart = Nothing }
    _ -> c

bufSetText :: Int -> [String] -> Buffer -> Buffer
bufSetText h xs cs = bufCorrectEndOfText h
                              cs { chText = xs, chLineNum = length xs }

