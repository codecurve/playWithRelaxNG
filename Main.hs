{-# LANGUAGE Arrows #-}
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Data.Fixed (Centi)
import Data.Text (Text, pack)


type Comment = Maybe Text
data Transaction = Deposit !Text !Centi !Comment
                 | Withdrawal !Text !Centi !Comment
                 deriving Show

make "deposit" = Deposit
make "withdrawal" = Withdrawal
make trans = error $ "Invalid transaction type: " ++ trans

select = getChildren >>>  hasName "transactions" >>> getChildren >>> isElem

transform = proc el -> do
  trans <- getName -< el
  account <- getAttrValue "account" -< el
  amount <- getAttrValue "amount" -< el
  comment <- withDefault (Just . pack ^<< getText <<< getChildren) Nothing -< el
  returnA -< (make trans) (pack account) (read amount) comment

process = arrIO print

handleError = getAttrValue a_source >>> arrIO (putStrLn . ("Error in document: " ++))

main :: IO ()
main = do
  runX $ readDocument [withRelaxNG "transactions.rng"] "transactions.xml"
         >>> ((select >>> transform >>> process) `orElse` handleError)
  return ()