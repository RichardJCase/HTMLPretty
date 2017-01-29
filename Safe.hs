module Safe where

safeNth :: [a] -> Int -> Maybe a
safeNth [] n = Nothing
safeNth lst n =
  if n < 0 then Nothing
  else
    if (length lst) > n
    then Just (lst!!n)
    else Nothing

safeDiv :: (Eq a) => (Fractional a) => a -> a -> Maybe a
safeDiv a 0 = Nothing
safeDiv a b = Just (a / b)

maybeCharToChar :: Maybe Char -> Char
maybeCharToChar mc = case mc of
  Nothing -> '\0'
  Just c -> c

maybeLstToLst :: Maybe [a] -> [a]
maybeLstToLst ml = case ml of
  Nothing -> []
  Just l -> l

maybeStrToStr :: Maybe String -> String
maybeStrToStr ms = case ms of
  Nothing -> ""
  Just s -> s

maybeNumToNum :: (Num a) => Maybe a -> a
maybeNumToNum mn = case mn of
  Nothing -> -1
  Just n -> n

maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool mb = case mb of
  Nothing -> False
  Just b -> b
