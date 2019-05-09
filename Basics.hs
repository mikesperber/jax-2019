data Produkt =
    Milch { milchLiter :: Double }
  | Kekse Int
  | Buch { buchTitel :: String, buchNetto :: Double }

mehrwertsteuer :: Produkt -> Double
mehrwertsteuer (Kekse anzahl) = ((fromIntegral anzahl) * 0.5) * 0.19
mehrwertsteuer (Buch _ netto) = netto * 0.07
mehrwertsteuer (Milch liter) = (liter * 0.8) * 0.07

billiger (Milch liter) faktor = Milch (liter * faktor)
billiger (Kekse anzahl) faktor = Kekse (floor ((fromIntegral anzahl) * faktor))
billiger (Buch titel netto) faktor = Buch (take 10 titel) (netto * faktor)

-- parens around type signature

data ProduktListe =
    KeinProdukt
  | MehrProdukte Produkt ProduktListe

-- naa

data List a =
    Nil
  | Cons a (List a)

mehrwertsteuern' :: List Produkt -> List Double
mehrwertsteuern' Nil = Nil
mehrwertsteuern' (Cons produkt produkte) =
  Cons (mehrwertsteuer produkt) (mehrwertsteuern' produkte)

mehrwertsteuern :: [Produkt] -> [Double]
mehrwertsteuern [] = []
mehrwertsteuern (produkt:produkte) =
  (mehrwertsteuer produkt) : (mehrwertsteuern produkte)

gesamtsteuer :: [Produkt] -> Double
gesamtsteuer [] = 0
gesamtsteuer (produkt:produkte) =
  (mehrwertsteuer produkt) + (gesamtsteuer produkte)

listSum :: [Double] -> Double
listSum [] = 0
listSum (x:xs) = x + (listSum xs)

produktName :: Produkt -> String
produktName (Milch {}) = "Milch"
produktName (Kekse _) = "Kekse"
produktName (Buch titel _) = titel

produktNamen :: [Produkt] -> [String]
produktNamen [] = []
produktNamen (produkt:produkte) =
  (produktName produkt) : (produktNamen produkte)

-- omit type signature
applyList f [] = []
applyList f (x:xs) = (f x) : (applyList f xs)

-- applyList (billiger 0.8) ...

gesamtsteuer' produkte = listSum (mehrwertsteuern produkte)
gesamtsteuer'' = listSum . (map mehrwertsteuer)

istBuch :: Produkt -> Bool
istBuch (Buch _ _) = True
istBuch _ = False

istMilch :: Produkt -> Bool
istMilch (Milch _) = True
istMilch _ = False

nurBuecher :: [Produkt] -> [Produkt]
nurBuecher [] = []
nurBuecher (produkt : produkte) =
   if istBuch produkt
   then produkt : (nurBuecher produkte)
   else nurBuecher produkte

nurMilch :: [Produkt] -> [Produkt]
nurMilch [] = []
nurMilch (produkt : produkte) =
   if istMilch produkt
   then produkt : (nurBuecher produkte)
   else nurBuecher produkte

listFilter p [] = []
listFilter p (x:xs) =
  if p x
  then x : (listFilter p xs)
  else listFilter p xs

-- Primzahlen aus einer Liste von Zahlen (1. Zahl ist Primzahl) herausfiltern
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =
  x : (sieve (strikeMultiples x xs))

-- Vielfache einer Zahl streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n xs = filter (\ x -> x `rem` n /= 0) xs

-- natürlichen Zahlen ab einer bestimmten produzieren
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

