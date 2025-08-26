qsort :: [Int] -> [Int]
qsort list = if length list == 0 
    then
        []
    else
        qsort [y | y <- calda, y <= cab, y < cab]
        ++ [cab]
        ++ qsort [y | y <- calda, y > cab]
        where
            calda = tail list
            cab = head list

