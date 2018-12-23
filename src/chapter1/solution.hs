{-
1.2-i
Q: Determine the cardinality of Either Bool (Bool, Maybe Bool) -> Bool.

Left Bool = 2
(Bool, Maybe Bool) = 2 * 3 = 6

2 ^ (2 + 6) = 256
-}


{-
1.4-i
Q: Use Curry–Howard to prove the exponent law that
a^b × a^c = a ^ b+c . That is, provide a function of the type
(b -> a) -> (c -> a) -> Either b c -> a and one of
(Either b c -> a) -> (b -> a, c -> a).
-}

productExample :: (b -> a) -> (c -> a) -> Either b c -> a
productExample fba _ (Left b) = fba b
productExample _ fca (Right c) = fca c

sumExample :: (Either b c -> a) -> (b -> a, c -> a)
sumExample f = (f . Left, f . Right)


{-
Exercise 1.4-ii
Prove (a × b)^c = a^c × b^c .
-}

prod14ii :: (c -> (a, b)) -> (c -> a, c -> b)
prod14ii f = (fst . f, snd . f)

sum14ii :: (c -> a) -> (c -> b) -> c -> (a, b)
sum14ii fca fcb = \c -> (fca c, fcb c)

{-
Exercise 1.4-iii
Give a proof of (a^b)^c = a^(b×c) . Does it remind you of
anything from Prelude?
-}

allExpon14iii :: (c -> b -> a) -> (b, c) -> a
allExpon14iii fcba = \(b, c) -> fcba c b

prodExpon14iii :: ((b, c) -> a) -> c -> b -> a
prodExpon14iii f = \c b -> f (b, c)
