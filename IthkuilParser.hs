module Ithkuil where
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (liftM)

data Word = Word {
      i :: String    -- cv
    , ii :: String   -- vl
    , iii :: String  -- cg/cs
    , iv :: String   -- vr 
    , v :: String    -- cx/cv
    , vi :: String   -- vp/vl
    , vii :: String  -- cr
    , viii :: String -- vc
    , ix :: String   -- "ci+vi"
    , x :: String    -- ca
    , xi :: [String] -- VxC
    , xii :: String  -- Vf
    , xiii :: String -- Cb
} deriving (Show)

word :: Parser Word
word = do 
    (part1, part2, part3, part4, part5, 
     part6, part7, part8, part9, part10) <- 
      (try wholeWord <|> halfWord)
    part11 <- many (try suffix)
    (part12, part13) <- option ("", "") spots12_13
    eof

    return $ Word part1 part2 part3 part4 part5 
                  part6 part7 part8 part9 part10
                  part11 part12 part13

wholeWord :: Parser (String, String, String, String, String,
                     String, String, String, String, String)
wholeWord = do
    (part1, part2, part3, part4, part5, part6) <-
        option ("", "", "", "", "", "") (try parts1_6)
    (part7, part8, part9, part10) <- parts7_10

    return (part1, part2, part3, part4, part5, 
            part6, part7, part8, part9, part10)

halfWord :: Parser (String, String, String, String, String,
                     String, String, String, String, String)
halfWord = do 
    (part7, part8, part9, part10) <- parts7_10
    return $ ("", "", "", "", "", "", part7, part8, part9, part10)

suffix :: Parser String
suffix = do
    vowel <- vowelCluster
    cons <- consonantCluster
    return $ vowel ++ cons

parts1_6 :: Parser (String, String, String, String, String, String) 
parts1_6 = do
    (part1, part2, part3, part4) <- option ("", "", "", "") (spots1234)
    (part5, part6) <- option ("", "") spots56
    return (part1, part2, part3, part4, part5, part6)

parts7_10 :: Parser (String, String, String, String)
parts7_10 = do
    part7 <- rootCluster 
    part8 <- caseVowel 
    part9 <- option "" spot9
    part10 <- consonantCluster
    return (part7, part8, part9, part10)

spots1234 :: Parser (String, String, String, String)
spots1234 = do
    (part1, part2, part3) <- option ("", "", "") (try spots123)
    part4 <- vowelCluster 
    return (part1, part2, part3, part4)

spots123 :: Parser (String, String, String)
spots123 = do
    (part1, part2) <- option ("", "") spots12
    part3 <- spot3Cluster 
    return (part1, part2, part3)

spots12 :: Parser (String, String)
spots12 = do
    part1 <- option "" rootCluster
    part2 <- vowelCluster
    return (part1, part2)

spots56 :: Parser (String, String)
spots56 = do
    part5 <- char '\'' >> consonantCluster
    part6 <- vowelCluster
    return (part5, part6)

spot9 :: Parser String
spot9 = do
    cons  <- ciCluster
    vowel <- vowelCluster
    return $ cons ++ vowel

spots12_13 :: Parser (String, String)
spots12_13 = do 
    part12 <- vowelCluster
    part13 <- option "" (char '\'' >> consonantCluster)
    return (part12, part13)

ciCluster :: Parser String
ciCluster = 
    choice $ (try.string) `map` ["hw", "h", "w", "y"]

spot3Cluster :: Parser String
spot3Cluster = try softCluster <|> syllabicCluster

softCluster :: Parser String
softCluster =
    choice $ (try.charThen) `map` [
            ('h', "rwhmn", True)
          , ('y', "", True)
          , ('w', "", True)
          , ('l', "wy", False)
          , ('r', "wy", False)
          , ('ř', "wy", False)
        ] where
            charThen (c, cs, aloneOK) = do
                rest <- char c >> 
                  if aloneOK
                   then option [] (stringOneOf cs)
                   else stringOneOf cs
                return $ c : rest
            stringOneOf cs = return `liftM` oneOf cs

syllabicCluster :: Parser String
syllabicCluster = do
        c <- consonant
        cs <- char '-' >> consonantCluster
        return $ c ++ '-' : cs

rootCluster :: Parser String
rootCluster = do
    notFollowedBy softCluster 
    consonantCluster

consonantCluster :: Parser String
consonantCluster = liftM (foldl (++) "") (many1 consonant)
-- combine all the consonant strings into one long string

vowelCluster :: Parser String
vowelCluster = many1 $ oneOf vowels where
    vowels = "aáàâeéèêëiíìîoóòôöuúùûü"

caseVowel :: Parser String
caseVowel = do
    vowels1 <- vowelCluster
    (apostrophe, vowels2) <- 
      option ("", "") (char '\'' >>= \x -> 
        option "" vowelCluster >>= \y ->
          return ([x], y))

    return $ vowels1 ++ apostrophe ++ vowels2

consonant :: Parser String
consonant = do
    first <- oneOf "pbtdkgqcčżjfvţszšžçxhļmnrňlwyř"
    if first `elem` "ptkqcč" 
       then do aspiration <- option "" $ try (string "ʰ") <|> try (string "'")
               return $ first : aspiration
       else if first == 'd' then do maybeH <- option "" $ try (string "h")
                                    return $ first : maybeH
                            else do return [first]

pw :: String -> Either ParseError Word
pw = parse word ""
