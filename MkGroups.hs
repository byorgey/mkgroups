{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens          (Lens', makeLenses, (&), (.~), (^.))
import           Control.Monad.Random
import           Data.List             (intercalate, isInfixOf, transpose)
import           Data.List.Split       (chunksOf, splitOn)
import           Options.Applicative
import           System.FilePath
import           System.Random.Shuffle

import           Data.Map              (Map)
import qualified Data.Map              as M

type Group = String
type Student = String
type Role = String
type Assignment = Map Group [(Maybe Role, Student)]

data GroupOpts = GroupOpts
  { _studentFile :: FilePath
  , _absent      :: [Student]
  , _groupFile   :: FilePath
  , _groups      :: Maybe [Group]
  , _roleFile    :: FilePath
  , _roles       :: Maybe [Role]
  , _numGroups   :: Maybe Int
  , _groupSize   :: Maybe Int
  }
  deriving Show

makeLenses ''GroupOpts

groupOpts :: Parser GroupOpts
groupOpts = GroupOpts
  <$> strOption
      (  long "students" <> short 's'
      <> metavar "FILE"
      <> showDefault
      <> value "students.txt"
      <> help "List of student names")
  <*> many (strOption
      (  long "absent" <> short 'a'
      <> metavar "STUDENT"
      <> help "(Partial) name of student who is absent"
      ))
  <*> strOption
      (  long "groupfile"
      <> metavar "FILE"
      <> showDefault
      <> value "groups.txt"
      <> help "List of group names")
  <*> (optional . fmap (splitOn ",") . strOption)
      (  long "groups" <> short 'g'
      <> metavar "LIST"
      <> help "Comma-separated list of groups")
  <*> strOption
      (  long "rolefile"
      <> metavar "FILE"
      <> showDefault
      <> value "roles.txt"
      <> help "List of roles")
  <*> (optional . fmap (splitOn ",") . strOption)
      (  long "roles" <> short 'r'
      <> metavar "LIST"
      <> help "Comma-separated list of roles")
  <*> (optional . fmap read . strOption)
      (  long "numgroups" <> short 'n'
      <> metavar "INT"
      <> help "Number of groups to generate"
      )
  <*> (optional . fmap read . strOption)
      (  long "groupsize" <> short 'z'
      <> metavar "INT"
      <> help "Size of each group"
      )

groupInfo :: ParserInfo GroupOpts
groupInfo = info (groupOpts <**> helper)
  (  fullDesc
  <> progDesc "Randomly assign students to groups"
  <> header "mkgroups - student group generator"
  )

main :: IO ()
main = mkGroups =<< execParser groupInfo

mkGroups :: GroupOpts -> IO ()
mkGroups opts = do
  studentList <- getStudents opts
  groupList   <- getGroups opts
  roleList    <- getRoles opts
  assign      <- assignGroups opts studentList groupList roleList
  printAssignment assign

getStudents :: GroupOpts -> IO [Student]
getStudents opts = (filter present . lines) <$> readFile (opts ^. studentFile)
  where
    present s = all (not . match s) (opts ^. absent)
    match s term = term `isInfixOf` s

getGroups :: GroupOpts -> IO [Group]
getGroups opts = listOrFile (opts ^. groups) (opts ^. groupFile)

getRoles :: GroupOpts -> IO [Role]
getRoles opts = listOrFile (opts ^. roles) (opts ^. roleFile)

listOrFile :: Maybe [String] -> FilePath -> IO [String]
listOrFile (Just xs) _ = return xs
listOrFile Nothing f   = lines <$> readFile f

assignGroups :: GroupOpts -> [Student] -> [Group] -> [Role] -> IO Assignment
assignGroups opts ss groupNames rs = do
  ss' <- shuffleM ss
  let gs   = transpose . chunksOf n $ ss'
      gsrs = map (zip (map Just rs ++ repeat Nothing)) gs
  return $ M.fromList (zip groupNames gsrs)
  where
    n = case (opts ^. numGroups, opts ^. groupSize) of
      (Just ng, _) -> ng
      (_, Just gs) -> length ss `div` gs
      _            -> length groupNames

printAssignment :: Assignment -> IO ()
printAssignment = putStr . formatAssignment

formatAssignment :: Assignment -> String
formatAssignment = intercalate "\n" . map formatGroup . M.assocs
  where
    formatGroup (g, ss) = unlines $
      [ g
      , replicate (length g) '-'
      ]
      ++
      map formatStudent ss
    formatStudent (r, s) = s ++ formatRole r
    formatRole Nothing  = ""
    formatRole (Just r) = " (" ++ r ++ ")"
