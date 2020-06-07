servers = [("Server of FMI", 120, [("A1", 25), ("Baza danni1", 30)]),("Server Vivacom", 300, [("A1", 25), ("B1", 40)]),
           ("Server of UNSS", 180,[("U1", 25), ("Baza danni1", 30)]),("Server of UASG", 120, [("A1", 25)])]


--a)
sN (x, _ , _) = x
sC (_, x, _) = x
sBD (_,_,x) = x

change (x,y,_) z = (x,y,z)

sumOfServer l = sumTakenMB (sBD l)
sumTakenMB bds = foldr (+) 0 onlyMB
    where onlyMB = map snd bds

percentTaken server = div (100*(sumOfServer server)) (sC server)

hasLargestDB [x] = x
hasLargestDB (x:xs) = hasLargestDBHelper xs startMin startServer
    where startMin = percentTaken x
          startServer = x

hasLargestDBHelper [] _ minServer = minServer
hasLargestDBHelper (x:xs) minPercent minServer = if (minPercent > percent)
                                              then hasLargestDBHelper xs percent x
                                              else hasLargestDBHelper xs minPercent minServer
    where percent = percentTaken x

--b)
freePercent server = (sC server) - (sumOfServer server)


checkDataB dataBase1 dataBase2 = checkDataBases dataBase1 dataBase2 []

checkDataBases [] _ result = result
checkDataBases server1DataBase@(x:xs) server2DataBase result
    |x `elem` server2DataBase = checkDataBases xs server2DataBase (x:result)
    |otherwise = checkDataBases xs server2DataBase result


takeThoseWhoAreNotTheSame serverDataBase dataBaseDublicates = filter (\x ->not (x `elem` dataBaseDublicates)) serverDataBase

changeServer server dublicates = change server (takeThoseWhoAreNotTheSame (sBD server) dublicates)

cleanHelper []          _     _ result = result
cleanHelper (x:xs)      []    l result = cleanHelper xs l l (x:result)
cleanHelper xss@(x:xs) (y:ys) l result
    |not $ null dublicates && freeX < freeY  = cleanHelper ((changeServer x dublicates):xs) ys l result
    |otherwise = cleanHelper xss ys l result
     where dublicates = checkDataB (sBD x) (sBD y) 
           dbX = sBD x
           dbY = sBD y
           freeX = freePercent x
           freeY = freePercent y

cleanDublicates l = cleanHelper l l l []

--task2
funcN f x 0 = x
funcN f x n = funcN f (f x) (n-1)

validNum x f l = if x `elem ` [(funcN f y m)| y <- l, m <- [0..x]] then True else False
nums f l = [x| x <- [1..],  validNum x f l == True]

--task3

data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

-- minBetween (Node Empty root Empty) _ _ n min  = root
-- minBetween (Node left root Empty) parent child n min =  if (child == parent) && root < min then minBetween left root child n min
-- minBetween (Node Empty root right) parent child n min = minBetween right parent child n min
-- minBetween (Node left root right) parent child n min = if (minBetween left root right )
