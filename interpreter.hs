type TokenName = String
type TokenValue = Char
type Token = (TokenName, TokenValue)
token_identify :: Char -> Token
token_identify '-' = ("MINUS", '-')
token_identify '+' = ("PLUS", '+')
token_identify '*' = ("MULTIPLE", '*')
token_identify '/' = ("DIVIDE", '/')
token_identify ' ' = ("SPACE", ' ')
token_identify '=' = ("EQUAL", '=')
token_identify '(' = ("SCOPE_L", '(')
token_identify ')' = ("SCOPE_R", ')')
token_identify c 
    | c >= '0' && c <= '9' = ("NUMBER", c)
    | ('a' <= c && c <= 'z') || (c >= 'A' && c <= 'Z') = ("LETTER", c)
token_identify _ = ("UNKNOWN", '?')

classify_tokens :: String -> [Token]
classify_tokens [] = []
classify_tokens (h:t) = token_identify h : classify_tokens t

firstAll :: [Token] -> [TokenName]
firstAll [] = []
firstAll (h:t) = fst h : firstAll t

secondAll :: [Token] -> [TokenValue]
secondAll [] = []
secondAll (h:t) = snd h : secondAll t

sub_list :: Int -> Int -> [a] -> [a]
sub_list start end list = take (end - start) (drop start list)

type TokenWord = String
identify_first_word :: [Token] -> (TokenWord, [Token])
identify_first_word [] = ("", [])
identify_first_word tokens_list@(h:t) =
    if fst h `elem` ["NUMBER", "LETTER"]
        then (secondAll groupedTokens, restOfTokens)
        else ([snd h], t)
    where 
        (groupedTokens, restOfTokens) = span (\(name, _) -> name == fst h) tokens_list

identify_words :: [Token] -> [TokenWord]
identify_words [] = []
identify_words tokens = current_word : identify_words rest
    where 
        (current_word, rest) = identify_first_word tokens

findIndexes :: String -> [String] -> [Int]
findIndexes target tokens = findIndexesInternal target tokens 0
  where
    findIndexesInternal _ [] _ = []
    findIndexesInternal t (h:xs) acc
        | h == t    = acc : findIndexesInternal t xs (acc + 1)
        | otherwise = findIndexesInternal t xs (acc + 1)  

give_scopes_index tokens = (hl, hr) : give_scopes_index_internal tl tr
    where
        give_scopes_index_internal [] [] = []
        give_scopes_index_internal token_left token_right = 
            (hl, hr) : give_scopes_index_internal tl tr
            where
                (hl:tl) = token_left
                (hr:tr) = token_right
        left_tokens = findIndexes "(" tokens
        right_tokens = findIndexes ")" tokens
        (hl:tl) = left_tokens
        (hr:tr) = reverse right_tokens

resolve_simple_math_operation [numero_final] = read numero_final :: Int
resolve_simple_math_operation [] = 0
resolve_simple_math_operation tokens = resolve_simple_math_operation ((show current_value):rest)
    where
        valid_tokens = filter (\x -> x /= " " && x /= "(" && x /= ")" ) tokens
        (fst_number:t1) = valid_tokens
        (operator:t2) = t1
        (sdn_number:rest) = t2
        int_first = read fst_number :: Int
        int_second = read sdn_number :: Int
        current_value = case operator of
            "+" -> int_first + int_second
            "-" -> int_first - int_second
            "*" -> int_first * int_second
            "/" -> int_first `div` int_second
            _   -> error "Operador inválido"

get_scope scope list = sub_list start end list
    where
        start = (fst scope)
        end = (snd scope)

replace_scope scope list solved = 
    take start list
    ++ solved
    ++ drop (end + 1) list
    where
        start = fst scope
        end = snd scope


identify_and_solve_middle_math_scope tokens =
        if not ("(" `elem` tokens) then tokens
        -- else scope_tokens
        else identify_and_solve_middle_math_scope solved_tokens
    where
        (scope:scope_rest) = reverse (give_scopes_index tokens)
        scope_tokens = get_scope scope tokens
        solved_scope = resolve_simple_math_operation scope_tokens
        solved_tokens = replace_scope scope tokens [show solved_scope]

resolve_math_operation tokens = resolve_simple_math_operation  (identify_and_solve_middle_math_scope tokens)

parse code = identify_words (classify_tokens code)

-- code = ["2", " ", "+", " ", "10", " ", "*", " ", "4"] -- 48
code = "(2 * (10 * (2 + 10))) / 2" -- 120
