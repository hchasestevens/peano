data Number = Zero | SuccessorOf Number 
	deriving (Show, Eq)


-- Addition:
plus :: Number -> Number -> Number
x `plus` Zero = x
x `plus` (SuccessorOf y) = SuccessorOf (x `plus` y)


-- Multiplication:
times :: Number -> Number -> Number
_ `times` Zero = Zero
x `times` (SuccessorOf y) = x `plus` (x `times` y)


-- Subtraction:
minus :: Number -> Number -> Number
x `minus` Zero = x
(SuccessorOf x) `minus` (SuccessorOf y) = x `minus` y


-- Comparison:
greaterThan :: Number -> Number -> Number
(SuccessorOf _) `greaterThan` Zero = (SuccessorOf Zero)
Zero `greaterThan` _ = Zero
(SuccessorOf x) `greaterThan` (SuccessorOf y) = x `greaterThan` y

equals :: Number -> Number -> Number
Zero `equals` Zero = SuccessorOf Zero
Zero `equals` _ = Zero
_ `equals` Zero = Zero
(SuccessorOf x) `equals` (SuccessorOf y) = x `equals` y


-- Division:
modulo :: Number -> Number -> Number
x `modulo` y 
	| SuccessorOf _ <- x `equals` y = Zero
	| Zero <- x `greaterThan` y = x
	| otherwise = (x `minus` y) `modulo` y

data RemainderNumber = Remainder Number
	deriving (Show)

type DivisionResult = (Number, RemainderNumber)

dividedBy :: Number -> Number -> DivisionResult
Zero `dividedBy` x = (Zero, Remainder Zero)
x `dividedBy` y = (SuccessorOf nextXDivisionResult, Remainder remainder)
	where
		remainder = x `modulo` y
		divisibleX = x `minus` remainder
		nextX = divisibleX `minus` y
		(nextXDivisionResult, _) = (nextX `dividedBy` y)


-- Exponentiation:
toThePowerOf :: Number -> Number -> Number
_ `toThePowerOf` Zero = SuccessorOf Zero
x `toThePowerOf` (SuccessorOf y) = x `times` (x `toThePowerOf` y)


root :: Number -> Number -> Number
_ `root` Zero = Zero
x `root` y = iterRoot y
	where
		iterRoot a@(SuccessorOf b)
			| SuccessorOf _ <- a `toThePowerOf` x `greaterThan` y = iterRoot b
			| otherwise = a

		

-- Main:
main :: IO ()
main = do
	-- Addition:
	let one = SuccessorOf Zero
	let two = SuccessorOf (SuccessorOf Zero)
	putStrLn "1 + 1 ="
	print $ one `plus` one

	-- Multiplication:
	putStrLn "2 * 2 ="
	print $ two `times` two
	
	-- Subtraction:
	let three = one `plus` two
	putStrLn "3 - 1 ="
	print $ three `minus` one
	putStrLn "3 - 2 ="
	print $ three `minus` two
	putStrLn "3 - 3 ="
	print $ three `minus` three
	
	-- Comparison:
	putStrLn "3 > 2 ?"
	print $ three `greaterThan` two
	putStrLn "2 > 3 ?"
	print $ two `greaterThan` three
	
	-- Division:
	let six = three `plus` three
	putStrLn "6 / 3 ="
	print $ six `dividedBy` three
	putStrLn "6 / 2 ="
	print $ six `dividedBy` two
	let five = six `minus` one
	putStrLn "6 / 5 ="
	print $ six `dividedBy` five

	-- Exponentiation:
	putStrLn "3 ** 2 ="
	print $ three `toThePowerOf` two
	let nine = three `times` three
	putStrLn "3 ** 2 == 9 ?"
	print $ three `toThePowerOf` two `equals` nine

	-- Root
	putStrLn "root(2, 9) ="
	print $ two `root` nine
	putStrLn "root(3, 27) ="
	print $ three `root` (nine `times` three)
