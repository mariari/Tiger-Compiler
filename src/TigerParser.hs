module TigerParser where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Monoid
import           Text.ParserCombinators.Parsec.Language
import           Control.Monad.Trans (lift)
import           Control.Monad.Identity
import qualified Text.ParserCombinators.Parsec.Token as T
import           Text.Parsec.Expr                    as E
import qualified Data.Symbol                         as S

import AbstractSyntax

langaugeDef :: GenLanguageDef String u Identity
langaugeDef = LanguageDef
              { T.reservedNames   = ["array", "if", "then", "else"
                                    ,"while", "for", "to", "do", "let"
                                    ,"in", "function", "var", "type"
                                    ,"import", "primitive"]
              , T.reservedOpNames = [",", ":", ";", "(", ")", "[", "]",
                                     "{", "}", ".", "+", "-", "*", "/",
                                     "=", "<>", "<", "<=", ">", ">=", "&", "|", ":="]
              , T.identStart      = letter <|> char '_'
              , T.identLetter     = alphaNum <|> char '_'
              , T.caseSensitive   = True
              , commentStart      = "/*"
              , commentEnd        = "*/"
              , nestedComments    = True
              , identStart        = letter <|> char '_'
              , identLetter       = alphaNum <|> oneOf "_'"
              , opStart           = opLetter langaugeDef
              , opLetter          = oneOf ":!#$%&*+./<=>?@\\^|-~"
              , commentLine       = ""
              }

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser langaugeDef

-- could make these
identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
integer    = T.integer    lexer
semi       = T.semi       lexer
semiSep    = T.semiSep    lexer
whiteSpace = T.whiteSpace lexer
comma      = T.comma      lexer
braces     = T.braces     lexer
brackets   = T.brackets   lexer
natural    = T.natural    lexer


sourceLineCol source = (sourceLine source, sourceColumn source)

getLineCol = fmap sourceLineCol getPosition

symbol :: Parser S.Symbol
symbol = S.intern <$> identifier

parseTigerLine = parseTigerLine' ""

parseTigerLine' = runParser (whiteSpace >> expression') ()

parseTigerFile :: FilePath -> IO (Either ParseError ExpI)
parseTigerFile fname = do
  input <- readFile fname
  return $ parseTigerLine' fname input

expression :: Parser ExpI
expression = buildExpressionParser optable expression' <?> "Exp"

expression' ::  Parser ExpI
expression' = seq'
           <|> (ifThen <?> "if then")
           <|> for
           <|> while
           <|> negation
           <|> (let' <?> "let expression")
           <|> break'
           <|> nil
           <|> try arrCreate
           <|> try recCreate
           <|> try funcall
           <|> try assign
           <|> lvalue
           <|> stringLit
           <|> intLit

dec :: Parser (Dec Identity)
dec =  tydec
   <|> vardec
   <|> fundec
   <?> "declaration"

tyP :: Parser (Ty Identity)
tyP =  arrty
   <|> recty
   <|> namety
   <?> "type creation"

-- ExpI------------------------------------------------------

seq' :: Parser ExpI
seq' = do
  pos <- getLineCol
  seq <- parens (expression `sepBy` semi)
  return $ Sequence seq pos

ifThen :: Parser ExpI
ifThen = do
  pos <- getLineCol
  reserved "if"
  pred <- expression
  reserved "then"
  then' <- expression
  else' <- optionMaybe (reserved "else" >> expression)
  case else' of
    Just x  -> return $ IfThenElse pred then' x pos
    Nothing -> return $ IfThen     pred then'   pos

for :: Parser ExpI
for = do
  pos <- getLineCol
  reserved "for"
  var <- symbol
  reservedOp ":="
  from <- expression
  reserved "to"
  end <- expression
  reserved "do"
  run <- expression
  let esc = Identity True
  return $ For var esc from end run pos

while :: Parser ExpI
while = do
  pos <- getLineCol
  reserved "while"
  pred <- expression
  reserved "do"
  run <- expression
  return $ While pred run pos

assign :: Parser ExpI
assign = do
  pos <- getLineCol
  lvalue <- lvalueParser
  reservedOp ":="
  exp <- expression'
  return $ Assign lvalue exp pos

negation = do
  pos <- getLineCol
  reservedOp "-"
  exp <- expression
  return $ Negation exp pos

let' = do
  pos <- getLineCol
  reserved "let"
  decs <- many1 dec
  reserved "in"
  exps <- expression `sepBy` semi -- 0 or more, so no sepBy1
  reserved "end"
  return $ Let decs exps pos

funcall = do
  pos <- getLineCol
  id  <- symbol
  exps <- parens (expression `sepBy` comma)
  return $ Funcall id exps pos

arrCreate = do
  pos <- getLineCol
  tyid <- symbol
  exp  <- brackets expression
  reserved "of"
  exp2 <- expression
  return $ ArrCreate tyid exp exp2 pos

recCreate = do
  pos <- getLineCol
  tyid   <- symbol
  fields <- braces (field' `sepBy` comma)
  return $ RecCreate tyid fields pos

field' :: Parser (Field Identity)
field' = do
  pos <- getLineCol
  id' <- symbol
  reservedOp "="
  exp <- expression
  return $ Field id' exp pos

break' = (getLineCol >>= return . Break) <* reserved "break"

intLit = do
  pos <- getLineCol
  int <- integer
  return $ IntLit (fromInteger int) pos


-- Haskells string has the same escape characters I think!

stringLit :: Parser ExpI
stringLit = do
  pos <- getLineCol
  char '"'
  string <- many (noneOf "\"")
  char '"'
  spaces
  return $ StringLit string pos

nil = (getLineCol >>= return . Nil) <* reserved "nil"

-- Lvalue---------------------------------------------------------------------------
-- The following section for lvalue is modified from stackoverflow
leftRec :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
leftRec p op = p >>= rest
  where
    rest x = (op >>= rest . ($ x)) <|> return x

lvalue :: Parser ExpI
lvalue = Var <$> lvalueParser

lvalueParser :: Parser (Var Identity)
lvalueParser = leftRec idParser (fieldExpI <|> subscript)
  where
    idParser = do
      pos <- getLineCol
      id' <- identifier
      return $ SimpleVar (S.intern id') pos

fieldExpI :: Parser (Var Identity -> Var Identity)
fieldExpI = do
  pos <- getLineCol
  reservedOp "."
  a <- identifier
  return $ (\l -> FieldVar l (S.intern a) pos)

subscript :: Parser (Var Identity -> Var Identity)
subscript = do
  pos <- getLineCol
  e <- brackets expression
  return (\l -> Subscript l e pos)

-- Typ----------------------------------------------------------------------
arrty :: Parser (Ty Identity)
arrty = do
  pos <- getLineCol
  reserved "array"
  reserved "of"
  sym <- symbol
  return $ ArrayTy sym pos

recty = RecordTy <$> braces (fieldDec `sepBy` comma)

namety = do
  pos <- getLineCol
  id' <- symbol
  return $ NameTy id' pos

-- Declarations--------------------------------------------------------------
fieldDec :: Parser (FieldDec Identity)
fieldDec = do
  pos <- getLineCol
  id' <- symbol
  reservedOp ":"
  typid  <- symbol
  let escape = Identity True
  return $ FieldDec id' escape typid pos -- Escape is set to true by default

tydec :: Parser (Dec Identity)
tydec = do
  pos <- getLineCol
  reserved "type"
  typid <- symbol
  reservedOp "="
  ty <- tyP
  return $ TypeDec typid ty pos

fundec :: Parser (Dec Identity)
fundec = do
  pos <- getLineCol
  reserved "function"
  id'    <- symbol
  fields <- parens (fieldDec `sepBy` comma)
  optional (reservedOp ":")
  mtypid <- optionMaybe symbol
  reservedOp "="
  exp <- expression
  return $ FunDec id' fields mtypid exp pos

vardec :: Parser (Dec Identity)
vardec = do
  pos <- getLineCol
  reserved "var"
  id' <- symbol
  optional (reservedOp ":")
  mtypid <- optionMaybe symbol
  reservedOp ":="
  exp <- expression
  let esc = Identity True
  return $ VarDec id' esc mtypid exp pos

-- ExpI parser for numbers

infixOp op pos exp1 exp2 = Infix' exp1 op exp2 pos

createInfix :: String -> Op -> Parser (ExpI -> ExpI -> ExpI)
createInfix opStr term = (getLineCol >>= return . infixOp term) <* reservedOp opStr

listToChoice :: [(String, Op)] -> Parser (ExpI -> ExpI -> ExpI)
listToChoice = choice . fmap (uncurry createInfix)

createOpTable term = Infix term AssocLeft

timesDiv    = listToChoice [("*", Times), ("/", Div)]
addMinus    = listToChoice [("+",  Plus), ("-", Minus)]
comparisons = listToChoice [("=", Eq), ("<>", Neq), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)]
checks1     = listToChoice [("|", Or)]
checks2     = listToChoice [("&", Or)]

optable = [[createOpTable timesDiv]
          ,[createOpTable addMinus]
          ,[createOpTable comparisons]
          ,[createOpTable checks1]
          ,[createOpTable checks2]]
