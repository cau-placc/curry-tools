{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Test.EasyCheck

-- Lookup with default rules:
nlookup key (_ ++ [(key,value)] ++ _) = Just value
nlookup'default _   _                 = Nothing 

main1 = nlookup 3 [] --> Nothing
main2 = nlookup 3 [(1,11),(3,14),(6,7)] --> Just 14
main3 = nlookup 3 [(1,11),(3,14),(6,7),(3,19)]  --> Just 14 | Just 19
main4 = nlookup 3 failed
main5 = nlookup () [((),1),(failed,2)] --> Just 1

test1 = nlookup 3 [] -=- (Nothing :: Maybe Int)
test2 = nlookup 3 [(1,11),(3,14),(6,7)] -=- Just 14
test3 = nlookup 3 [(1,11),(3,14),(6,7),(3,19)]  <~> (Just 14 ? Just 19)
test4 = failing $ (nlookup 3 failed :: Maybe Int)
test5 = nlookup () [((),1),(failed,2)] -=- Just 1
