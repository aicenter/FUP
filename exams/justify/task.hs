import Data.List (intercalate)


pad :: [String] -> Int -> String
pad [] _ = []
pad (w:[]) maxWidth = w ++ replicate (maxWidth - length w) ' '
pad words@(w:ws) maxWidth = (intercalate lspace bw) ++ rights where

  ceilDiv :: (Integral b, Integral a1, Integral a2) => a1 -> a2 -> b
  ceilDiv x y = ceiling ((fromIntegral x) / (fromIntegral y))

  spaces = maxWidth - sum (map length words)
  words_to_pad = length ws
  rspace_len = quot spaces words_to_pad
  lspace_len = ceilDiv spaces words_to_pad
  rspace = replicate rspace_len ' '
  lspace = replicate lspace_len ' '
  ltimes = spaces - (words_to_pad * rspace_len)
  (bw, lw) = splitAt (ltimes+1) words
  rights = if (length lw == 0)
             then ""
             else rspace ++ (intercalate rspace lw)

breakWords :: [String] -> Int -> ([String], [String])
breakWords words maxWidth = _breakWords words maxWidth 0 [] where
  _breakWords [] _ _ acc = (reverse acc, [])
  _breakWords words@(w:ws) maxWidth currWidth acc =
    if (currWidth + wlen) > maxWidth 
       then (reverse acc, words)
       else _breakWords ws maxWidth (currWidth + wlen + 1) (w:acc) where
         wlen = length w


breakAll :: [String] -> Int -> [[String]]
breakAll words maxWidth = (line:lines) where
  lines = if length rest == 0 
     then []
     else breakAll rest maxWidth
  (line, rest) = breakWords words maxWidth


justify :: Int -> [String] -> [String]
justify maxWidth words = justified ++ [lastFilled] where
  brokenLines = breakAll words maxWidth
  justified = map (\r -> pad r maxWidth) (init brokenLines)
  lastLine = intercalate " " (last brokenLines)
  lastFilled = lastLine ++ replicate (maxWidth - length lastLine) ' '


printJustified :: Int -> [String] -> IO ()
printJustified w = do (mapM_ putStrLn) . justify w

main = do
  let mw1 = 16
      ws1 = ["This", "is", "an", "example", "of", "text", "justification."]

      mw2 = 16
      ws2 = ["What","must","be","acknowledgment","shall","be"]

      mw3 = 20
      ws3 = ["Science","is","what","we"
            ,"understand","well"
            ,"enough","to","explain","to"
            ,"a","computer.","Art","is"
            ,"everything","else","we","do"]

  printJustified mw1 ws1
