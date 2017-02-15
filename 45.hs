pull a@(x:xs) b@(y:ys) c@(z:zs)
  | x == y && y == z = x
  | x == s = pull xs b c 
  | y == s = pull a ys c 
  | z == s = pull a b zs 
  where s = minimum [x, y, z]

main = print $ pull (s 286 t) (s 166 p) (s 144 h)
  where 
    s i f = f <$> [i..]
    t n = n * (n+1) `div` 2
    p n = n * (3*n-1) `div` 2
    h n = n * (2*n-1)
