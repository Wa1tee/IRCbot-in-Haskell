--IRC-bot by Antti "Waitee" Auranen
--1.3.2017
import Data.List
import Network
import System.IO 
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Data.List.Split
import Data.Either
import Text.Read

server   = "irc.cc.tut.fi"
port     = 6667
chan     = "#vessadeeku"
nick     = "deekubot"

data Either a b = String Int
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
      disconnect = hClose . socket
      loop st    = runReaderT run st

connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where  
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :Bot by Waitee")
    write "JOIN" chan
    write "PRIVMSG" (chan ++ " :ei juku :D")
    asks socket >>= listen

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
    master x  = ":Waitee!waitee@kapsi.fi"

eval :: String -> Net ()
eval       "!quit"                 = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x |   "!id " `isPrefixOf` x   = privmsg (drop 4 x)
eval       "!vim"                  = privmsg "kovipu: graafiset editorit on n00beille :D" 
eval       "!uptime"               = uptime >>= privmsg
eval x |   "!sum " `isPrefixOf` x  = if summaa (drop 5 x) == Nothing
                                        then privmsg "Parse error"
                                        else privmsg $ drop 5 $ show $ summaa $ drop 5 x
eval _                             = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

io :: IO a -> Net a
io = liftIO

uptime :: Net String
uptime = do
  now  <- io getClockTime
  zero <- asks starttime
  return . pretty $ diffClockTimes now zero

--returns (up)time in a prettier format
pretty :: TimeDiff -> String
pretty td = 
  unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
      where 
        merge (tot, acc) (sec, typ) = let (sec', tot') = divMod tot sec
                                           in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                    foldl' merge (tdSec td, []) metrics


summaa :: String -> Maybe Int 
summaa x = fmap sum $ sequence $ readNumbers x
  

readNumbers :: String -> [Maybe Int]
readNumbers x = map readMaybe $ words x :: [Maybe Int]

