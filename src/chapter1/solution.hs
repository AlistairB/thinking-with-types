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
a b × a c = a b+c . That is, provide a function of the type
(b -> a) -> (c -> a) -> Either b c -> a and one of
(Either b c -> a) -> (b -> a, c -> a).
-}

productExample :: (b -> a) -> (c -> a) -> Either b c -> a
productExample fba _ (Left b) = fba b
productExample _ fca (Right c) = fca c

sumExample :: (Either b c -> a) -> (b -> a, c -> a)
sumExample f = (f . Left, f . Right)
