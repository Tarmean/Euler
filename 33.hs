import Data.Ratio
main = print $ product valids
valids = do
  stricken <- [1..9]
  (top, top') <- pairings stricken
  (bot, bot') <- pairings stricken
  let combined = top' % bot'
  if combined < 1 && combined == top % bot
  then return combined
  else mempty

pairings s = do
  o <- [1..9]
  [(o, s * 10 + o), (o, s + o * 10)]
