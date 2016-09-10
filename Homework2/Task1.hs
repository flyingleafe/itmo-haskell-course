module Task1 where

safeTail, safeInit :: [a] -> Either String [a]
safeTail [] = Left "List is empty, no head"
safeTail (_:xs) = Right xs
safeInit [] = Left "List is empty, no tail"
safeInit [_] = Right []
safeInit (x:xs) = (x:) <$> safeInit xs

strip :: [a] -> [a]
strip ls = case safeTail ls >>= safeInit of
             Left _ -> []
             Right xs -> xs
