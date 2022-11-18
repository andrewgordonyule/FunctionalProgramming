import Text.Parsec

-- Simon's datatype for expressions
data BinOp = Add | Sub | Mul | Div deriving (Show,Eq)

data ArithExpr =
      Compound ArithExpr BinOp ArithExpr
    | Value Int          
    deriving Show


numberParser:: Parsec String st Int
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = read <$> (many1 digit)

operatorParser:: Parsec String st BinOp
operatorParser = selectOp <$> (oneOf "+-*/")
                   where selectOp '+' = Add
                         selectOp '-' = Sub
                         selectOp '*' = Mul
                         selectOp '/' = Div

expressionParser:: Parsec String st ArithExpr
expressionParser = (between (char '(') (char ')') binaryExpressionParser) <|>
                   (Value <$> numberParser)

binaryExpressionParser:: Parsec String st ArithExpr
binaryExpressionParser = Compound <$> expressionParser <*> operatorParser <*> expressionParser



-- now run GHCi> parseTest expressionParser "(1+1)"
-- or GHCi> parse expressionParser "error" "(1+(2*3))"
