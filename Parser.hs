module Parser (
    regex
) where
import Regex
import Text.ParserCombinators.Parsec

combRegex :: (Regex a -> Regex a -> Regex a) -> Regex a -> Maybe (Regex a) -> Regex a
combRegex f r Nothing = r
combRegex f r (Just r') = f r r'

regex :: Parser (Regex Char)
regex = do x <- regex1
           y <- optionMaybe $ char '|' >> regex
           return $ combRegex Alt x y
                    

regex1 :: Parser (Regex Char)
regex1 = do x <- regex2
            y <- optionMaybe $ char '&' >> regex1
            return $ combRegex Inter x y

regex2 :: Parser (Regex Char)
regex2 = do x <- regex3
            y <- optionMaybe regex2
            return $ combRegex Seq x y

regex3 :: Parser (Regex Char)
regex3 = do x <- regex4
            y <- optionMaybe $ oneOf "?+*"
            return $ case y of
                       Nothing    -> x
                       (Just '*') -> Rep x
                       (Just '+') -> Seq x (Rep x)
                       (Just '?') -> Alt Empty x

parenthesis :: Parser (Regex Char)
parenthesis = do char '('
                 x <- regex <?> "regex"
                 char ')'
                 return x

regex4 :: Parser (Regex Char)
regex4 = parenthesis <|>
         ((char '\\' <?> "\"\\\"") >>
              fmap Term (oneOf "|&!()*?+$\\" <?> "one of |&!()*?+$\\")) <|>
         (char '!' >> fmap Comp regex) <|>
         (char '$' >> return Empty) <|>
         fmap Term (noneOf "|&!()*?+$\\" <?> "a character literal")

