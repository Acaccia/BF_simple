module Main where
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Word                     (Word8)
import           Text.ParserCombinators.Parsec
import           Unsafe.Coerce                 (unsafeCoerce)

main :: IO ()
main = do (_ : l : _) <- fmap read . words <$> getLine
          input <- getLine
          program <- mconcat <$> replicateM l getLine
          case (parse parseBf "brainfuck" program) of
            Left err -> print err
            Right p  -> evalBf input p >> putStrLn ""

data Zipper = Z [Word8] Word8 [Word8] deriving Show

left, right, increment, decrement :: Zipper -> Zipper
left (Z (l:ls) v rs) = Z ls l (v : rs)
left (Z [] v rs)     = Z [] 0 (v : rs)
right (Z ls v (r : rs)) = Z (v : ls) r rs
right (Z ls v [])       = Z (v : ls) 0 []
increment (Z ls v rs) = Z ls (v+1) rs
decrement (Z ls v rs) = Z ls (v-1) rs

output :: Zipper -> Char
output (Z _ v _) = unsafeCoerce v

store :: Char -> Zipper -> Zipper
store c (Z ls _ rs) = Z ls (unsafeCoerce c) rs

data Instruction = Inc | Dec | Plus | Minus | Dot | Comma | Comment | Loop [Instruction] deriving Show

parseBf :: Parser [Instruction]
parseBf = many1 $ Inc <$ char '>' <|> Dec <$ char '<' <|> Plus <$ char '+'
               <|> Minus <$ char '-' <|> Dot <$ char '.' <|> Comma <$ char ','
               <|> Loop <$> between (char '[') (char ']') (try parseBf <|> return []) <|> Comment <$ noneOf "]"

evalBf :: String -> [Instruction] -> IO ()
evalBf input program = () <$ evalStateT (traverse go program) (Z [] 0 [], input)
  where go :: Instruction -> StateT (Zipper, String) IO ()
        go Inc = modify' (first right)
        go Dec = modify' (first left)
        go Plus = modify' (first increment)
        go Minus = modify' (first decrement)
        go Dot = gets (output . fst) >>= liftIO . putChar
        go Comma = get >>= \(z, i:is) -> put (store i z, is)
        go (Loop l) = gets (fromEnum . output . fst) >>= \v -> when (v /= 0) (traverse go l >> go (Loop l))
        go _ = return ()
