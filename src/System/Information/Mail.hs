module System.Information.Mail (
    MailConfig(MailConfig)
    , initMail
    , getMail
    , hookMail
    , unpackMail
    , getSubject
    , defaultMailConfig
    ) where

import System.Directory
import System.FilePath
import System.INotify

import Control.Monad
import Control.Concurrent.STM
import Data.List hiding (delete)
import qualified Data.Set as S
import qualified Data.List as L
import Text.ParserCombinators.Parsec

data MailConfig = MailConfig [(
    String -- Name of the inbox
    , FilePath -- Path to the maildir
    )]

defaultMailConfig :: MailConfig
defaultMailConfig = MailConfig [("Mail", "~/mail")]
-- Create a bunch of mailboxes
initMail :: MailConfig -> IO [TVar (S.Set String)]
initMail (MailConfig pairs) = mapM (const $ newTVarIO S.empty) pairs

-- Get info from mailboxes
getMail :: [TVar (S.Set String)] -> IO [[String]]
getMail vs = atomically $ mapM (fmap S.toList . readTVar) vs

expandHome :: String -> IO String
expandHome = return

-- Each time a mailbox updates, run this hook. This is blooking so fork it.
hookMail :: MailConfig -> ([(String, [String])] -> IO ())-> [TVar (S.Set String)] -> IO ()
hookMail (MailConfig pairs) f vs = do
    let newmail = map ((</> "new") . snd) pairs
        inboxes = map fst pairs
        ev = [Move, MoveIn, MoveOut, Create, Delete]
    ds <- mapM expandHome newmail
    i <- initINotify
    zipWithM_ (\dir var -> addWatch i ev dir (handle var)) ds vs

    forM_ (zip ds vs) $ \(d, v) -> do
        s <- fmap (S.fromList . filter (not . isPrefixOf "."))
            $ getDirectoryContents d
        atomically $ modifyTVar v (S.union s)

    changeLoop (mapM (fmap S.toList . readTVar) vs) $ \mails ->
        f (zipWith (,) inboxes mails)

-- like forever but blocks until the value in the STM have changed
changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)

handle :: TVar (S.Set String) -> Event -> IO ()
handle v e = atomically $ modifyTVar v $ case e of
    Created  {} -> create
    MovedIn  {} -> create
    Deleted  {} -> delete
    MovedOut {} -> delete
    _           -> id
 where
    delete = S.delete (filePath e)
    create = S.insert (filePath e)

-- Takes a path to a mail, reads it and starts parsing it
unpackMail:: FilePath -> IO (Either ParseError ([(String, String)], [String]))
unpackMail path = do
    file <- readFile path
    return $ parse parseMail path file

parseMail :: GenParser Char st ([(String, String)], [String])
parseMail = do
    header <- many headerLine
    _ <- emptyLine
    body <- many bodyLine
    eof
    return $ (header, body)

eol :: GenParser Char st Char
eol = char '\n'
colon :: GenParser Char st Char
colon = char ':'
emptyLine:: GenParser Char st Char
emptyLine = char '\n'

headerLine :: GenParser Char st (String, String)
headerLine = do
    title <- many (noneOf ":\n")
    _ <- colon
    value <- many (noneOf "\n")
    _ <- eol
    return (title, value)

bodyLine :: GenParser Char st String
bodyLine = do
    value <- many (noneOf "\n")
    _ <- eol
    return value

-- Takes a parsed mail and tries to find the header
getSubject :: Either ParseError ([(String, String)], [String]) -> Maybe (String, String)
getSubject (Left _) = Nothing
getSubject (Right (header, _)) = L.find (\(sub, _) -> sub == "Subject") header
