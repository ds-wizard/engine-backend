module Util.List where

import Model.Context.AppContext
import Model.Error.Error

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

switchMaybeAndList :: [Maybe a] -> Maybe [a]
switchMaybeAndList = foldl go (Just [])
  where
    go (Just l) (Just u) = Just $ l ++ [u]
    go _ Nothing = Nothing
    go Nothing _ = Nothing

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
  where
    rdHelper seen [] = seen
    rdHelper seen (x:xs)
      | x `elem` seen = rdHelper seen xs
      | otherwise = rdHelper (seen ++ [x]) xs

elems :: Eq a => [a] -> [a] -> Bool
elems (x:xs) list = x `elem` list && xs `elems` list
elems ([]) list = True

generateList :: Int -> [Int]
generateList size = [0 .. (size - 1)]

foldMaybesInContext :: [AppContextM (Either AppError (Maybe a))] -> AppContextM (Either AppError [a])
foldMaybesInContext = Prelude.foldl foldOne (return . Right $ [])
  where
    foldOne ::
         AppContextM (Either AppError [a])
      -> AppContextM (Either AppError (Maybe a))
      -> AppContextM (Either AppError [a])
    foldOne eitherListIO eitherMaybeEntityIO = do
      eitherList <- eitherListIO
      eitherMaybeEntity <- eitherMaybeEntityIO
      case eitherList of
        Right list ->
          case eitherMaybeEntity of
            Right maybeEntity ->
              case maybeEntity of
                Just entity -> return . Right $ list ++ [entity]
                Nothing -> return . Right $ list
            Left error -> return . Left $ error
        Left error -> return . Left $ error
