{-# LANGUAGE LambdaCase #-}
module Parser where

import Builtins
import Codepage
import Debug
import DecompressString
import Expr
import PrattParser
import Text.Parsec
import Text.Parsec.Char
import Control.Monad (forM)
import qualified Data.Map as Map
import Data.List (elemIndex,(\\))
import Data.Maybe (catMaybes)

-- Parser state
data PState = PState
  { varStack  :: [ELabel]
  , varSupply :: Int
  , numLines  :: Int
  , unmarked  :: [Int]
  , parenCtr  :: Int
  }

-- Parser type
type Parser = Parsec String PState

-- Unwrapped parser, giving strings for errors
parseExpr :: String -> Either String [Exp [Lit Scheme]]
parseExpr str = case runParser multiline initState "" str of
  Left err -> Left $ show err
  Right val -> Right val
  where initState = PState [] 0 0 [] 0

-- Generate and push a new expression variable
pushNewVar :: Parser ELabel
pushNewVar = do
  stat <- getState
  let var = "x" ++ show (varSupply stat)
  putState stat{varStack = var : varStack stat,
                varSupply = varSupply stat + 1}
  return $ trace' ("pushed " ++ var) var

-- Generate and append a new expression variable
appendNewVar :: Parser ELabel
appendNewVar = do
  stat <- getState
  let var = "x" ++ show (varSupply stat)
  putState stat{varStack = varStack stat ++ [var],
                varSupply = varSupply stat + 1}
  return $ trace' ("appended " ++ var) var

-- Peek at a variable from the stack; extend stack if necessary
peekVar :: Int -> Parser ELabel
peekVar ix = do
  stack <- varStack <$> getState
  let len = length stack
  if ix >= len
    then do
      vars <- forM [0..ix-len] $ const appendNewVar
      return $ trace' ("peeked " ++ show ix ++ " from " ++ show stack ++ ", got " ++ show (last vars)) $ last vars
    else return $ trace' ("peeked " ++ show ix ++ " from " ++ show stack ++ ", got " ++ show (stack !! ix)) $ stack !! ix

-- Pop a variable off the stack
popVar :: Parser ()
popVar = do
  stat <- getState
  putState stat{varStack = trace' ("popping from " ++ show (varStack stat)) tail $ varStack stat}

-- Parse a right paren or be at end of line
rParen :: Parser ()
rParen = (char ')' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof

-- Eat a lone space character (not followed by sub- or superscipt)
soleSpace :: Parser ()
soleSpace = try $ char ' ' >> notFollowedBy (oneOf "⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉")

-- List of builtins applied to overflowing line numbers
lineFuncs :: [Exp [Lit Scheme]]
lineFuncs = [bins "argdup",
             bins "flip",
             bins "map",
             bins "zip",
             bins "hook"]

-- Parse a multiline expression, where some lines are marked with a leading space
multiline :: Parser [Exp [Lit Scheme]]
multiline = do
  lineExprs <- sepEndBy1 (try markedLine <|> unmarkedLine) endOfLine
  numLn <- numLines <$> getState
  unmarkeds <- unmarked <$> getState
  return $ map (updateLineNums numLn unmarkeds) lineExprs
  where markedLine = do
          soleSpace
          expr <- lineExpr
          stat <- getState
          putState stat{numLines = numLines stat + 1}
          return (True, expr)
        unmarkedLine = do
          expr <- lineExpr
          stat <- getState
          putState stat{unmarked = unmarked stat ++ [numLines stat],
                        numLines = numLines stat + 1}
          return (False, expr)
        updateLineNums numLn unmark (marked, expr) = go expr
          where numUnmark = length unmark
                go (ELine n)
                  | marked, lNum <- mod n numLn, rounds <- div n numLn =
                      case rounds of 0 -> ELine lNum
                                     k -> EApp (lineFuncs !! (k-1)) $ ELine lNum
                  | lNum <- mod n numUnmark, rounds <- div n numUnmark =
                      case rounds of 0 -> ELine (unmark !! lNum)
                                     k -> EApp (lineFuncs !! (k-1)) $ ELine (unmark !! lNum)
                go (EApp e1 e2) = EApp (go e1) (go e2)
                go (EOp e1 e2 e3) = EOp (go e1) (go e2) (go e3)
                go (EAbs name exp) = EAbs name (go exp)
                go (ELet name exp body) = ELet name (go exp) (go body)
                go e = e

-- Parse a line of Husk code
lineExpr :: Parser (Exp [Lit Scheme])
lineExpr = do
  state <- getState
  putState state{varStack = []}
  expr <- expression
  overflowVars <- reverse . varStack <$> getState
  let lambdified = foldr EAbs expr overflowVars
  return $ trace' (show lambdified) lambdified

-- Parse an expression
expression :: Parser (Exp [Lit Scheme])
expression = mkPrattParser opTable term
  where term = between (char '(') rParen expression
            <|> builtin
            <|> try float
            <|> integer
            <|> character
            <|> str
            <|> comprstr
            <|> intseq
            <|> lambda
            <|> try lambdaArg
            <|> subscript
            <|> list

        opTable = [[InfixL $ optional soleSpace >> return (EOp invisibleOp)]]
        invisibleOp = bins "com4 com3 com2 com app"

-- Parse a builtin
builtin :: Parser (Exp [Lit Scheme])
builtin = do
  label <- oneOf commands
  return $ cmd label

-- Parse an integer
integer :: Parser (Exp [Lit Scheme])
integer = do
  digits <- many1 digit
  return $ ELit [Value digits numType]
  where numType = Scheme [] $ CType [] $ TConc TNum

-- Parse a float
float :: Parser (Exp [Lit Scheme])
float = do
  prefix <- many digit
  char '.'
  suffix <- many digit
  case (prefix,suffix) of
    ("","") -> return $ ELit [Value "0.5" numType]
    (_ ,"") -> return $ ELit [Value (prefix ++ ".5") numType]
    ("", _) -> return $ ELit [Value ("0." ++ suffix) numType]
    (_ , _) -> return $ ELit [Value (prefix ++ "." ++ suffix) numType]
  where numType = Scheme [] $ CType [] $ TConc TNum

-- Parse a character
character :: Parser (Exp [Lit Scheme])
character = do
  quote <- char '\''
  c <- anyChar
  return $ ELit [Value (show c) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a string
str :: Parser (Exp [Lit Scheme])
str = do
  quote <- char '"'
  s <- content
  quote2 <- (char '"' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof
  return $ ELit [Value (show s) $ Scheme [] $ CType [] $ TList (TConc TChar)]
  where
    content = do
      codedText <- many $ noneOf "\"\n\\"
      plainText <- return $ map decode codedText
      maybeEscape <- optionMaybe $ char '\\' >> anyChar
      case maybeEscape of
        Nothing -> return plainText
        Just c -> do plainText2 <- content; return $ plainText++c:plainText2
    decode '¶' = '\n'
    decode '¨' = '"'
    decode '¦' = '\\'
    decode  c  =  c

-- Parse a compressed string
comprstr :: Parser (Exp [Lit Scheme])
comprstr = do
  quote <- char '¨'
  s <- content
  quote2 <- (char '¨' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof
  return $ ELit [Value (show $ s) $ Scheme [] $ CType [] $ TList (TConc TChar)]
  where
    content = do
      comprText <- many $ noneOf "¨\n"
      decomprText <- return $ decompressString comprText
      return $ map decode decomprText
    decode '¶' = '\n'
    decode  c  =  c

-- Parse an integer sequence
intseq :: Parser (Exp [Lit Scheme])
intseq = do
  iseqCommand <- char 'İ'
  seqId <- anyChar
  return $ EApp (bins "intseq") $ ELit [Value (show seqId) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a generalized lambda
lambda :: Parser (Exp [Lit Scheme])
lambda = do
  lam <- oneOf "λμξφψχ"
  let numArgs = case lam of
        'λ' -> 1
        'μ' -> 2
        'ξ' -> 3
        'φ' -> 1
        'ψ' -> 2
        'χ' -> 3
  expr <- iterate lambdify expression !! numArgs
  rParen
  return $ if lam `elem` "φψχ" then EApp (bins "fix") expr else expr
  where
    lambdify parser = do
      var <- pushNewVar
      expr <- parser
      popVar
      return $ EAbs var expr

-- Parse a lambda argument
lambdaArg :: Parser (Exp [Lit Scheme])
lambdaArg = do
  supStr <- try (pure <$> oneOf sups) <|> (char ' ' >> many1 (oneOf sups))
  let digits = catMaybes $ (`elemIndex` sups) <$> supStr
      supNum = foldl1 (\n d -> 10*n + d) digits
  var <- peekVar supNum
  return $ EVar var
  where sups = "⁰¹²³⁴⁵⁶⁷⁸⁹"

-- Parse a subscript; used as line numbers
subscript :: Parser (Exp [Lit Scheme])
subscript = do
  subStr <- try (pure <$> oneOf subs) <|> (char ' ' >> many1 (oneOf subs))
  let digits = catMaybes $ (`elemIndex` subs) <$> subStr
      subNum = foldl1 (\n d -> 10*n + d) digits
  return $ ELine subNum
  where subs  = "₀₁₂₃₄₅₆₇₈₉"


-- Intermediate type for list comprehension parsing
data DoExp = Ex String | Gd String deriving (Show,Eq)

-- Parse a list comprehension
list :: Parser (Exp [Lit Scheme])
list = do
  l <- oneOf "[]"
  -- Preprocess statements, to reorder & parse in order
  retStmt <- token
  doStmts <- many $ oneOf "∟Ŀ" >>= \case
                      '∟' -> Ex <$> token
                      'Ŀ' -> Gd <$> token
  rParen

  rem <- getInput -- Save remaining source

  -- Parse the statements in the order that they would
  -- appear in a do-notation
  (vars,bind) <- processDos doStmts

  ret' <- if null retStmt then return (bins "id")
                          else do setInput retStmt *> expression

  mapM_ (const popVar) vars

  setInput rem -- Restore remaining source

  -- Apply return statement to all variables in case of [ or an empty return
  -- TODO: better default for ']' and empty return
  let ret = if l == '[' || null retStmt then foldl EApp ret' (map EVar vars)
                                        else ret'
  return $ bind (EApp (bins "pure") ret)

  where -- Keep strings for later processing; consumes everything
        -- until ∟,Ŀ or ) is reached (ignoring nested list comprehensions)
        token :: Parser String
        token = parenCtr <$> getState >>= \case
          0 -> done <|> recurse
          _ -> recurse

        done = lookAhead (oneOf "∟Ŀ" *> return () <|> rParen) *> return []

        recurse = do c  <- oneOf (codepage \\ "\n") >>= \case
                             x | x `elem` open -> modifyCtr succ *> return x
                               | x == ')'      -> modifyCtr pred *> return x
                               | otherwise     -> return x
                     cs <- token
                     return (c:cs)

        open  = "λμξφψχ[]("
        modifyCtr f = modifyState (\st -> st { parenCtr = f (parenCtr st) })

        -- Build up nested lambda-expressions
        processDos :: [DoExp] -> Parser ([ELabel],(Exp [Lit Scheme] -> Exp [Lit Scheme]))
        processDos (Ex d:ds) = do
          setInput d
          exp <- expression <* eof
          var <- pushNewVar
          (vs,f) <- processDos ds
          return $ (var:vs, (\x-> EOp (bins "bindL") exp (EAbs var x)) . f)

        processDos (Gd d:ds) = do
          setInput d
          exp' <- expression <* eof
          (vs,f) <- processDos ds
          let exp = EApp (bins "guardL") exp'
          return $ (vs, (\x-> EOp (bins "bindL") exp (EApp (bins "const") x)) . f)

        processDos [] = return ([],id)
