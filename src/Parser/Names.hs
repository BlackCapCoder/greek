module Parser.Names where


import AST
import Parser.Parser


keyword :: String -> Parser ()
keyword word =
  token $ string word *> someSpace


-- May occur at any position in an identifier, except as the first letter
identifierChar = asum
  [ oneOf "_'"
  , alphaNum
  ]


name :: Parser Name =
  token $ liftA2 (:)
    do lower <|> oneOf "_"
    do many identifierChar

typeName :: Parser Name =
  token $ liftA2 (:)
    do upper
    do many identifierChar

someName :: Parser (Kind, Name)
someName = asum
  [ (TYPE,) <$> typeName
  , (TERM,) <$> name
  ]

tyvar = name

