---
outline: deep
title: "Exam Task: Text Justification"
---

# Text Justification

Given an array of strings words and a width `maxWidth`, format the text such that each line has
exactly `maxWidth` characters and is fully (left and right) justified.

You should pack your words in a greedy approach; that is, pack as many words as you can in each
line. Pad extra spaces `' '` when necessary so that each line has exactly `maxWidth` characters.

Extra spaces between words should be distributed as evenly as possible. If the number of spaces on a
line does not divide evenly between words, the empty slots on the left will be assigned more spaces
than the slots on the right.

For the last line of text, it should be left-justified, and no extra space is inserted between
words.

Note:

* A word is defined as a character sequence consisting of non-space characters only.
* Each word's length is guaranteed to be greater than 0 and not exceed maxWidth.
* The input array words contains at least one word.

## Examples

```haskell
Input:
    words = ["This", "is", "an", "example", "of", "text", "justification."]
    maxWidth = 16
Output:
[
   "This    is    an",
   "example  of text",
   "justification.  "
]
```

```haskell
Input:
    words = ["What","must","be","acknowledgment","shall","be"]
    maxWidth = 16
Output:
[
  "What   must   be",
  "acknowledgment  ",
  "shall be        "
]
```
Explanation: Note that the last line is `"shall be    "` instead of `"shall     be"`, because the
last line must be left-justified instead of fully-justified.
Note that the second line is also left-justified because it contains only one word.

```haskell
Input: words = ["Science","is","what","we"
               ,"understand","well","enough"
               ,"to","explain","to","a"
               ,"computer.","Art","is"
               ,"everything","else","we","do"],
maxWidth = 20
Output:
[
  "Science  is  what we",
  "understand      well",
  "enough to explain to",
  "a  computer.  Art is",
  "everything  else  we",
  "do                  "
]
```


Constraints:

* `1 <= words.length <= 300`
* `1 <= words[i].length <= 20`
* `words[i]` consists of only English letters and symbols.
* `1 <= maxWidth <= 100`
* `words[i].length <= maxWidth`


[This task is taken from LeetCode.](https://leetcode.com/problems/text-justification/)


## Racket


Your file should be called `justify.rkt`, and `provide` the `justify` function.

::: details Exam Solution
```racket
#lang racket

(require racket/match)

(define (total-chars words)
  (foldl + 0 (map string-length words)))

(define (pad words maxWidth)
  (let ((spaces (- maxWidth (foldl + 0 (map string-length words)))))
    (cond
     [(null? words) null]
     [(= 1 (length words))
      (string-append (car words) (make-string (- maxWidth (string-length (car words))) #\space))]
     [else
      (let*-values (((words-to-pad) (length (cdr words)))
		    ((lspace-len) (quotient spaces words-to-pad))
		    ((lspace) (make-string lspace-len #\space))
		    ((bspace-len) (ceiling (/ spaces words-to-pad)))
		    ((bspace) (make-string bspace-len #\space))
		    ((btimes) (- spaces (* words-to-pad lspace-len)))
		    ((bw lw) (split-at words (add1 btimes))))
	(string-append
	 (string-join bw bspace)
	 (if (null? lw) "" (string-join lw lspace #:before-first lspace))))])))


(define (pad-last words maxWidth)
  (let ((trail-space (- maxWidth (+ (sub1 (length words)) (total-chars words)))))
    (string-join words " " #:after-last (make-string trail-space #\space))))


(define (break words maxwidth [curr-width 0] [acc '()])
  (if (empty? words)
      (list (reverse acc) words)
      (let* ((wlen (string-length (car words))))
        (if
         (> (+ curr-width wlen) maxwidth)
         (list (reverse acc) words)
         (break (cdr words)
                maxwidth
                (+ 1 curr-width wlen)
                (cons (car words) acc))))))


(define (break-all words max-width)
  (define res (break words max-width))
  (define row (car res))
  (define ws (cadr res))
  (cons row (if (empty? ws) ws (break-all ws max-width))))


(define (justify lst max-width)
  (define (spaces n) (make-string n #\space))
  (define broken-lines (break-all lst max-width))
  (define butlast (take broken-lines (- (length broken-lines) 1)))
  (define justif (map (lambda (r) (pad r max-width)) butlast))
  (define last-line (string-join (last broken-lines)))
  (define last-filled (string-append
                        last-line
                        (spaces (- max-width (string-length last-line)))))
  (append justif (list last-filled)))


(define words1 '("This" "is" "an" "example" "of" "text" "justification."))
(justify words1 16)
(map string-length (justify words1 16))

(define words2 '("What" "must" "be" "acknowledgment" "shall" "be"))
(justify words2 16)
(map string-length (justify words2 16))

(define words3 '("Science" "is" "what" "we"
                 "understand" "well" "enough"
                 "to" "explain" "to" "a" "computer."
                 "Art" "is" "everything" "else" "we" "do"))
(justify words3 20)
(map string-length (justify words3 20))
```
:::

::: details Exam Solution
```racket
#lang racket

(require racket/match)

(define (total-chars words)
  (foldl + 0 (map string-length words)))

(define (pad words maxWidth)
  (let ((spaces (- maxWidth (foldl + 0 (map string-length words)))))
    (cond
     [(null? words) null]
     [(= 1 (length words))
      (string-append (car words) (make-string (- maxWidth (string-length (car words))) #\space))]
     [else
      (let*-values (((words-to-pad) (length (cdr words)))
		    ((lspace-len) (quotient spaces words-to-pad))
		    ((lspace) (make-string lspace-len #\space))
		    ((bspace-len) (ceiling (/ spaces words-to-pad)))
		    ((bspace) (make-string bspace-len #\space))
		    ((btimes) (- spaces (* words-to-pad lspace-len)))
		    ((bw lw) (split-at words (add1 btimes))))
	(string-append
	 (string-join bw bspace)
	 (if (null? lw) "" (string-join lw lspace #:before-first lspace))))])))


(define (pad-last words maxWidth)
  (let ((trail-space (- maxWidth (+ (sub1 (length words)) (total-chars words)))))
    (string-join words " " #:after-last (make-string trail-space #\space))))


(define (justify words maxWidth)
  (match words
    ['() ""]
    [(list a ...)
     #:when (<= (+ (sub1 (length a)) (foldl + 0 (map string-length a))) maxWidth)
     (list (pad-last a maxWidth))]
    [(list a ..1 b ..1)
     #:when (<= (+ (sub1 (length a)) (foldl + 0 (map string-length a))) maxWidth)
     (cons (pad a maxWidth) (justify b maxWidth))]))


(define words1 '("This" "is" "an" "example" "of" "text" "justification."))
(justify words1 16)
(map string-length (justify words1 16))

(define words2 '("What" "must" "be" "acknowledgment" "shall" "be"))
(justify words2 16)
(map string-length (justify words2 16))

(define words3 '("Science" "is" "what" "we"
                 "understand" "well" "enough"
                 "to" "explain" "to" "a" "computer."
                 "Art" "is" "everything" "else" "we" "do"))
(justify words3 20)
(map string-length (justify words3 20))
```
:::


## Haskell

Your file should be called `Justify.hs`, contain a module of the same name, and export the `justify` function.

::: details Exam Solution
```haskell
module Justify (justify) where
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
```
:::
