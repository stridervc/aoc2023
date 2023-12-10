module Day08
  ( solve
  ) where

import qualified Text.Parsec as P
import qualified Data.Map.Strict as M

import Control.Monad.State

import Helpers

type Node = String
type NodeMap = M.Map Node (Node, Node)

data WalkState = WalkState
  { instructions  :: String
  , instrStart    :: String
  , currNode      :: !Node
  , currNodes     :: ![ Node ]
  , nodeMap       :: NodeMap
  , stepCount     :: !Int
  } deriving (Eq, Show)

newWalkState :: String -> NodeMap -> WalkState
newWalkState instr nm = WalkState
  { instructions  = instr
  , instrStart    = instr
  , currNode      = "AAA"
  , currNodes     = filter (\k -> last k == 'A') (M.keys nm)
  , nodeMap       = nm
  , stepCount     = 0
  }

dayParser :: Parser (String, NodeMap)
dayParser = do
  instr <- P.manyTill P.anyChar P.newline
  P.newline
  nodes <- P.many parseNodeLine
  return (instr, M.fromList nodes)
  where parseNode     = P.count 3 P.anyChar
        parseNodeLine = do
          key <- parseNode
          P.string " = ("
          left <- parseNode
          P.string ", "
          right <- parseNode
          P.char ')'
          P.newline
          return (key, (left, right))

popInstr :: State WalkState Char
popInstr = do
  instrsC <- gets instructions
  instrsB <- gets instrStart
  let instrsA = if null instrsC then instrsB else instrsC
  modify' (\s -> s { instructions = tail instrsA } )
  return $ head instrsA

lookupNode :: Node -> State WalkState (Node, Node)
lookupNode node = do
  nm <- gets nodeMap
  case M.lookup node nm of
    Just res  -> return res
    Nothing   -> error "lookupNode failed"

atEnd :: State WalkState Bool
atEnd = do
  node <- gets currNode
  if node == "ZZZ" then return True else return False

atEnds :: State WalkState Bool
atEnds = do
  nodes <- gets currNodes
  return $ all (\n -> last n == 'Z') nodes

incStep :: State WalkState Int
incStep = do
  count <- gets stepCount
  modify' (\s -> s { stepCount = count + 1 })
  return $ count + 1

step :: State WalkState Int
step = do
  instr <- popInstr
  node <- gets currNode
  paths <- lookupNode node
  let next = if instr == 'L' then fst paths else snd paths
  modify' (\s -> s { currNode = next })
  incStep

stepMulti :: State WalkState Int
stepMulti = do
  instr <- popInstr
  nodes <- gets currNodes
  paths <- mapM lookupNode nodes
  let nexts = if instr == 'L' then map fst paths else map snd paths
  modify' (\s -> s { currNodes = nexts })
  incStep

run :: State WalkState Int
run = do
  end <- atEnd
  if end
    then gets stepCount
    else step >> run

runMulti :: State WalkState Int
runMulti = do
  ends <- atEnds
  if ends
    then gets stepCount
    else stepMulti >> runMulti

part1 :: (String, NodeMap) -> IO ()
part1 (instr, nm) = print $ evalState run ws
  where ws  = newWalkState instr nm

part2 :: (String, NodeMap) -> IO ()
part2 (instr, nm) = print $ evalState runMulti ws
  where ws  = newWalkState instr nm

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
