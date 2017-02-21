-- https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md

import Control.Monad

data NFA q s = NFA {
        initialState :: q
      , isAccepting  :: q -> Bool
      , transition   :: q -> s -> [q]
    }

testNFA :: NFA q s -> [s] -> Bool
testNFA (NFA i a t) = any a . foldM t i

data State  = Q1 | Q2 | Q3 | Q4 | Q5 deriving (Eq, Show)
data Symbol = A | B | C | D deriving (Eq, Show)

-- initial state
i = Q1

-- accept criteria
a = (`elem` [Q4,Q5]) 
a' = (`elem` [Q4,Q5,Q1]) 

-- state transitions
t Q1 A = [Q2]
t Q2 A = [Q3,Q4]
t Q2 B = [Q1,Q2]
t Q2 C = [Q3,Q4]
t Q3 D = [Q5]
t Q4 A = [Q2,Q4]
t _  _ = []

nfa = NFA i a t 
nfa' = NFA i a' t 

main = do
  print $ testNFA nfa [A,B,C,D]
  print $ testNFA nfa [A,A,B,B]
  print $ testNFA nfa [A,A,A,B]
  print $ testNFA nfa' [A,A,A,B]

