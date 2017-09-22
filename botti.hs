--IRC-bot by Antti "Waitee" Auranen
--1.3.2017
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf


server   = "irc.cc.tut.fi"
port     = 6667
chan     = "#vessadeeku"
nick     = "deekubot"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
      disconnect = hClose . socket
      loop st    = runReaderT run st

connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
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

eval :: String -> Net ()
--eval       "!quit"                 = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x |   "!id " `isPrefixOf` x   = privmsg (drop 4 x)
eval       "!vim"                  = privmsg "kovipu: graafiset editorit on n00beille :D" 
--eval h x 
--eval x |   "!sum " `isPrefixOf` x  = privmsg (sum (tail(split x " ")))
eval       _                       = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

io :: IO a -> Net a
io = liftIO

--split :: Eq a => a -> [a] -> [[a]]
--split d [] = []
--split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


