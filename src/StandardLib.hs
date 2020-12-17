-- consumed, pattern, string -> (consumed, unconsumed)
-- depreacted by stripPrefix
consumeFront :: String -> String -> (String, String)
consumeFront pattern from = consumeFrontHelper [] pattern from
  where
    consumeFrontHelper :: String -> String -> String -> (String, String)
    consumeFrontHelper consumed [] ys = (consumed, ys)
    consumeFrontHelper consumed (x:xs) (y:ys) 
      | x == y = consumeFrontHelper (consumed ++ [x]) xs ys
      | otherwise = ([], (y:ys))