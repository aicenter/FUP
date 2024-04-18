module Task4 (rps) where
import Data.List

isFinished xs = null xs || any null xs

currentStrategies = map head

futureStrategies = map tail

view xs bs = map fst $ filter snd $ zip xs bs

eliminated throws =
    let e = elim $ sort $ nub throws
    in map (/= e) throws
    where
        elim ['p', 'r'] = 'r'
        elim ['p', 's'] = 'p'
        elim ['r', 's'] = 's'
        elim _ = ' '

rps :: [String] -> [[Char]] -> [String]
rps players strategies | isFinished strategies = players
rps players strategies =
    let current = currentStrategies strategies
        future = futureStrategies strategies
        mask = eliminated current
    in rps (view players mask) (view future mask)