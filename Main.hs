import Control.Exception
import Prelude hiding (catch)

data Info = Info String -- xxx ???
            deriving Show

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
          deriving Show

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

dummyInfo :: Info
dummyInfo = Info "dummyInfo"

eval1 :: Term -> Term
eval1 (TmIf _ (TmTrue _) t2 _) = t2
eval1 (TmIf _ (TmFalse _) _ t3) = t3
eval1 (TmIf fi t1 t2 t3) = let t1' = eval1 t1
                           in TmIf fi t1' t2 t3
eval1 (TmSucc fi t1) = let t1' = eval1 t1
                       in TmSucc fi t1'
eval1 (TmPred _ (TmZero _)) = TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ nv1))
  | isNumericVal nv1 = nv1
eval1 (TmPred fi t1) = let t1' = eval1 t1
                       in TmPred fi t1'
eval1 (TmIsZero _ (TmZero _)) = TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ nv1))
  | isNumericVal nv1 = TmFalse dummyInfo
eval1 (TmIsZero fi t1) = let t1' = eval1 t1
                         in TmIsZero fi t1'
eval1 _ = error "Error"

eval :: Term -> IO Term
eval t = handle handler $ do let t' = eval1 t
                             eval t'
  where
    handler :: SomeException -> IO Term
    handler _ = return t

main :: IO ()
main = print $ eval1 $ eval1 (TmIf (Info "if") (TmTrue $ Info "true") (TmZero $ Info "Zero1") (TmZero $ Info "Zero2"))
