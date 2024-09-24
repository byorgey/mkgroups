{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (Lens', makeLenses, (&), (.~), (^.))
import Control.Monad.Random
import Data.List (intercalate, isInfixOf, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Options.Applicative
import System.FilePath
import System.IO (hFlush, stdout)
import System.Random.Shuffle

type Group = String
type Student = String
type Role = String
type Assignment = Map Group [(Maybe Role, Student)]

data GroupOpts = GroupOpts
  { _studentFile :: FilePath
  , _absent :: [Student]
  , _groupFile :: FilePath
  , _groups :: Maybe [Group]
  , _roleFile :: FilePath
  , _roles :: Maybe [Role]
  , _numGroups :: Maybe Int
  , _groupSize :: Maybe Int
  , _dnpFile :: FilePath
  , _doNotPair :: [(Student, Student)]
  }
  deriving (Show)

makeLenses ''GroupOpts

groupOpts :: Parser GroupOpts
groupOpts =
  GroupOpts
    <$> strOption
      ( long "students"
          <> short 's'
          <> metavar "FILE"
          <> showDefault
          <> value "students.txt"
          <> help "List of student names"
      )
    <*> many
      ( strOption
          ( long "absent"
              <> short 'a'
              <> metavar "STUDENT"
              <> help "(Partial) name of student who is absent"
          )
      )
    <*> strOption
      ( long "groupfile"
          <> metavar "FILE"
          <> showDefault
          <> value "groups.txt"
          <> help "List of group names"
      )
    <*> (optional . fmap (splitOn ",") . strOption)
      ( long "groups"
          <> short 'g'
          <> metavar "LIST"
          <> help "Comma-separated list of groups"
      )
    <*> strOption
      ( long "rolefile"
          <> metavar "FILE"
          <> showDefault
          <> value "roles.txt"
          <> help "List of roles"
      )
    <*> (optional . fmap (splitOn ",") . strOption)
      ( long "roles"
          <> short 'r'
          <> metavar "LIST"
          <> help "Comma-separated list of roles"
      )
    <*> (optional . fmap read . strOption)
      ( long "numgroups"
          <> short 'n'
          <> metavar "INT"
          <> help "Number of groups to generate"
      )
    <*> (optional . fmap read . strOption)
      ( long "groupsize"
          <> short 'z'
          <> metavar "INT"
          <> help "Size of each group"
      )
    <*> strOption
      ( long "donotpairfile"
          <> metavar "FILE"
          <> showDefault
          <> value "donotpair.txt"
          <> help "List of forbidden pairs, one comma-separated pair per line"
      )
    <*> many
      ( (fmap readPair . strOption)
          ( long "donotpair"
              <> short 'd'
              <> metavar "PAIR"
              <> help "Comma-separated pair of (partial) student names who should not be paired"
          )
      )

readPair :: String -> (Student, Student)
readPair = (\[a, b] -> (a, b)) . splitOn ","

groupInfo :: ParserInfo GroupOpts
groupInfo =
  info
    (groupOpts <**> helper)
    ( fullDesc
        <> progDesc "Randomly assign students to groups"
        <> header "mkgroups - student group generator"
    )

main :: IO ()
main = mkGroups =<< execParser groupInfo

mkGroups :: GroupOpts -> IO ()
mkGroups opts = do
  studentList <- getStudents opts
  groupList <- getGroups opts
  roleList <- getRoles opts
  dnpList <- getDNP opts
  assign <- assignGroups opts studentList groupList roleList dnpList
  printAssignment assign

getStudents :: GroupOpts -> IO [Student]
getStudents opts = filter present . lines <$> readFile (opts ^. studentFile)
 where
  present s = (not . any (match s)) (opts ^. absent)
  match s term = term `isInfixOf` s

getGroups :: GroupOpts -> IO [Group]
getGroups opts = listOrFile (opts ^. groups) id (opts ^. groupFile)

getRoles :: GroupOpts -> IO [Role]
getRoles opts = listOrFile (opts ^. roles) id (opts ^. roleFile)

nonEmpty [] = Nothing
nonEmpty xs = Just xs

getDNP :: GroupOpts -> IO [(Student, Student)]
getDNP opts = listOrFile (nonEmpty (opts ^. doNotPair)) readPair (opts ^. dnpFile)

listOrFile :: Maybe [a] -> (String -> a) -> FilePath -> IO [a]
listOrFile (Just xs) _ _ = return xs
listOrFile Nothing rd f = map rd . lines <$> readFile f

assignGroups :: GroupOpts -> [Student] -> [Group] -> [Role] -> [(Student, Student)] -> IO Assignment
assignGroups opts ss groupNames rs forbidden = do
  ss' <- shuffleM ss
  let gs = transpose . chunksOf n $ ss'
      gsrs = map (zip (map Just rs ++ repeat Nothing)) gs
  if groupsOK forbidden gs
    then return $ M.fromList (zip groupNames gsrs)
    else assignGroups opts ss groupNames rs forbidden
 where
  n = case (opts ^. numGroups, opts ^. groupSize) of
    (Just ng, _) -> ng
    (_, Just gs) -> length ss `div` gs
    _ -> length groupNames

groupsOK :: [(Student, Student)] -> [[Student]] -> Bool
groupsOK forbidden = all (\grp -> all (\(s1, s2) -> not ((s1 `studentIn` grp) && (s2 `studentIn` grp))) forbidden)

studentIn :: Student -> [Student] -> Bool
studentIn s = any (s `isInfixOf`)

printAssignment :: Assignment -> IO ()
printAssignment = putStr . formatAssignment

formatAssignment :: Assignment -> String
formatAssignment = intercalate "\n" . map formatGroup . M.assocs
 where
  formatGroup (g, ss) =
    unlines $
      [ g
      , replicate (length g) '-'
      ]
        ++ map formatStudent ss
  formatStudent (r, s) = s ++ formatRole r
  formatRole Nothing = ""
  formatRole (Just r) = " (" ++ r ++ ")"
