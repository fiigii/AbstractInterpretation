module AbstractInterpreter where

import LabeledAst
import Data.Set (Set, member, union, unions)
import qualified Data.Set as Set
import Data.Map (Map, singleton, (!), insert)
import qualified Data.Map as Map
import Control.Monad.State
import Parser

data Closure = Closure LAst ContextEnvironment
             deriving (Eq, Ord)
data CFAState = CFAState LAst ContextEnvironment Store Context
              deriving (Eq, Ord, Show)

type Context = [Label]
type Variable = String
type ContextEnvironment = Map Variable Context
type Bind = (Variable, Context)
type Store = Map Bind (Set Closure)
type Cache = Map Label (Map Context (Set Closure))

type AbstractInterpreter = State (Set CFAState, Cache)

k :: Int
k = 1

k_CFA :: String -> (LAst, Cache, Store)
k_CFA source = case jsparse source of
  Right ts -> let (lAst, _) = convert ts
                  (states, cache) = execState (analysis Map.empty Map.empty [] lAst) (Set.empty, Map.empty)
                  stores = Set.foldl (\acc (CFAState _ _ s _) -> Set.insert s acc) Set.empty states
                  store = Map.unionsWith union $ Set.toList stores
              in (lAst, cache, store)
  Left e -> error $ show e 

analysis :: ContextEnvironment -> -- ce
            Store ->              -- store
            Context ->            -- delta
            LAst ->               -- expr
            AbstractInterpreter (Set Closure)
analysis ce store curr (Var x l) = let v = store ! (x, ce ! x)
                                   in do updateCache l curr v
                                         return v
analysis ce _ curr f@(Function _ _ l) =
     let freeVars = fv f
         ce' = Map.filterWithKey (\key _ -> key `member` freeVars) ce
         closure = Set.singleton (Closure f ce')
     in do updateCache l curr closure
           return closure

analysis ce store curr call@(Application e1 e2 l) =
  do seen <- getSeen
     let theState = CFAState call ce store curr
     if not $ theState `member` seen
       then do addSeen theState
               v1 <- analysis ce store curr e1
               v2 <- analysis ce store curr e2
               let next = nextContext curr l
                   each (Closure (Function x body _) env) =
                     let newCe = insert x next env
                         newStore = insert (x, next) v2 store
                     in analysis newCe newStore next body
               vs' <- mapM each $ Set.toList v1
               let v' = unions vs'
               updateCache l curr v'
               return v'
       else return Set.empty   
     
analysis ce store curr (IfExpr cond e1 e2 l) =
  do analysis ce store curr cond
     v1 <- analysis ce store curr e1
     v2 <- analysis ce store curr e2
     let v = v1 `union` v2
     updateCache l curr v
     return v

analysis ce store curr (LetRec bindings body l) =
  do let vars =  map fst bindings
         newCeElems = map (\x -> (x,curr)) vars
         newCePart = Map.fromList newCeElems
         newCe = ce `Map.union` newCePart
         newBinds = map (\x -> (x, curr)) vars
     vs <- mapM (analysis newCe store curr) (map snd bindings)
     let newStoreElems = zip newBinds vs
         newStore = store `Map.union` Map.fromList newStoreElems
     v <- analysis newCe newStore curr body
     updateCache l curr v
     return v

analysis ce store curr (BinaryExpr _ e1 e2 _) =
  do analysis ce store curr e1
     analysis ce store curr e2
     return Set.empty

analysis _ _ _ _ = return Set.empty

updateCache :: Label -> Context -> Set Closure -> AbstractInterpreter ()
updateCache l curr value =
  do cache <- getCache
     let newC = cache `merge` (singleton l $ singleton curr value)
     setCache newC
 
merge :: Cache -> Cache -> Cache
merge = Map.unionWith Map.union

getCache :: AbstractInterpreter Cache
getCache = do (_, c) <- get
              return c

setCache :: Cache -> AbstractInterpreter ()
setCache newC = do (seen, _) <- get
                   put (seen, newC)
getSeen :: AbstractInterpreter (Set CFAState)
getSeen = do (s,_) <- get
             return s

setSeen :: Set CFAState -> AbstractInterpreter ()
setSeen newSeen = do (_, c) <- get
                     put (newSeen, c)

addSeen :: CFAState -> AbstractInterpreter ()
addSeen new = do seen <- getSeen
                 setSeen (Set.insert new seen)

nextContext :: Context -> Label -> Context
nextContext callString l = take k $ l : callString

instance Show Closure where
  show (Closure (Function x _ l) env) = "Closure function (" ++ x ++ ") ... @" ++ show l ++ " bind " ++ show (Map.toList env)
