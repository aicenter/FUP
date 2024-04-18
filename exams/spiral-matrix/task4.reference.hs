module Task4 ( spiralMatrix ) where
type Matrix = [[Int]]

matAdd xss n = (map . map) (+ n) xss

wrap x ys z = x:ys++[z]

spiralMatrix 1 = [[1]]
spiralMatrix n = extendV $ extendH $ matAdd smaller (4*n-4) where
        smaller = spiralMatrix (n - 2)
        extendH x = zipWith3 wrap [4*n-4,4*n-5..3*n-1] x [n+1..2*n-2]
        extendV x = wrap [1..n] x [3*n-2,3*n-3..2*n-1]
