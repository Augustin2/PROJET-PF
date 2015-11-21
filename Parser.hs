 module Parser(
                Parser,
                caractere,
                parse,
                (|||),
                parserChars,
                unOuPlus,
                zeroOuPlus
             ) 
    where

    import Control.Applicative (Applicative(..))
    import Control.Monad       (liftM, ap)
  
    

    type Resultat a = Maybe (a, String)
    newtype Parser a = MkParser (String -> Resultat a) 

    echoue :: Parser a 
    echoue = MkParser (\_ -> Nothing)

    retourne :: a -> Parser a
    retourne v = MkParser (\s -> Just (v, s))

    caractere :: Parser Char
    caractere = MkParser (\s -> case s of
                              ""     -> Nothing
                              (c:cs) -> Just (c, cs))

    parse :: Parser a -> String -> Resultat a
    parse (MkParser p) = p

    (|||) :: Parser a -> Parser a -> Parser a
    p ||| p' = MkParser (\s -> case parse p s of
                                 Nothing -> parse p' s
                                 r       -> r)

    (>>>) :: Parser a -> (a -> Parser b) -> Parser b
    p >>> pf = MkParser (\s -> case parse p s of
                                 Nothing      -> Nothing
                                 Just (x, s') -> parse (pf x) s')

    -- Le lignes prochaines si ghc < 7.10
    {--
    instance Monad Parser where
        (>>=) = (>>>)
        return = retourne
        fail _ = echoue 
	     --} 

    -- Le lignes prochaines si ghc >= 7.10
    -- voir https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
    instance Functor Parser where
        fmap = liftM
    instance Applicative Parser where
        pure  = retourne {- move the definition of `return` from the `Monad` instance here -}
        (<*>) = ap
    instance Monad Parser where
        return = pure -- redundant since GHC 7.10 due to default impl
        (>>=) = (>>>)
        fail _ = echoue


    parserChars :: [Char] -> Parser Char
    parserChars str = MkParser s
            where
                s (x:xs) = if elem x str then Just(x,xs) else Nothing
                s _ = Nothing

    unOuPlus :: Parser a -> Parser [a]
    unOuPlus p = 
        (
            do
                x <- p
                xs <- zeroOuPlus p
                return (x:xs)
        )
        |||
        ( do x <- p ; return [x] )

    zeroOuPlus :: Parser a -> Parser [a]
    zeroOuPlus  p =
        unOuPlus p
        |||
        return []



