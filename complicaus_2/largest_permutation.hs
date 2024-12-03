{-# LANGUAGE LambdaCase #-}

import Data.IntMap.Strict as M
import Data.List as L
import Data.Maybe
import Data.Tuple
import Text.Read

keysDesc = fmap fst . M.toDescList

swapKV = M.fromList . fmap swap . M.toList

solve k ns = fmap snd . M.toList . swapKV . L.foldl' swapElems indicesOfN . zip ns . take k . keysDesc $ indicesOfN
    where indicesOfN = M.fromList $ zip ns [0..]

swapElems m (n, x) = M.update (const $ M.lookup n m) x $ M.update (const $ M.lookup x m) n m

(>>>) = flip (.)

parse :: String -> Maybe (Int, [Int])
parse = lines >>> fmap words >>> \case
    ((_:k:_):ns:_) -> (,) <$> readMaybe k
                          <*> traverse readMaybe ns
    _              -> Nothing

run = fmap (uncurry solve) . parse

main = interact $ maybe "Invalid input" (unwords . fmap show) . run
