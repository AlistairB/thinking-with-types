module ChapterEight.Solution where

{-
Exercise 8.2-i
What is the role signature of Either a b?

Representational - a b are applied to (->) with Left and Right, so become representational

type role Either representational representational
-}

{-
Exercise 8.2-ii
What is the role signature of Proxy a?

Phantom

type role Proxy phantom
-}
