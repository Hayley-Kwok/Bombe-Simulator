{-  Name: Hayley Wing Yin Kwok
    COM 2108 Functional Programming Assignment 2 : The Enigma Machine, Cribs and Menus-}

module Enigma where
  import AssignmentHelp
  import Data.Char
  import Data.List
  
  type Rotor = String
  type Reflector = [(Char,Char)]
  type Offsets = (Int,Int,Int)
  type Steckerboard = [(Char,Char)]
  --          (original message, encrypted message)
  type Crib = (String,String)
  type Menu = [Int]
  -- SimpleEnigma is Enigma without Steckerboard
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard
  
  data Tree a = Empty | Node Int [Tree a] deriving Show
  type Match = (Int,Int)

  alphabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  enigma4 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0) [('B','F'),('D','E'),('L','Q'),('U','T'),('P','M'),('I','C')])
  enigma5 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')])
  enigma6 = (SteckeredEnigma rotor1 rotor2 rotor4 reflectorB (0,0,1) [('F','T'),('D','U'),('V','A'),('K','W')])
  enigma7 = (SteckeredEnigma rotor1 rotor2 rotor5 reflectorB (0,0,0) [('H','Z'),('I','X')])

  {-------------------------------------------------Assignment 2 Code (Engima) -------------------------------------------------}

  -- This function takes in a character and Enigma and return the corresponding encrypted character
  -- Assumptions & Validations
  -- It is assumed that user will supplied an upper case character for encryption
  --  and there will be no repeat elements in the supplied Steckerboard and reflectors.
  -- There are validation for the rotors, the length of reflector and offsets. 
  -- Also, a simple check if the length of the Steckerboard is smaller than 11.
  enigmaEncode :: Char -> Enigma -> Char
  enigmaEncode plain (SimpleEnigma lr mr rr reflector origOff) 
    = simpleEnigmaEncode plain (SimpleEnigma lr mr rr reflector origOff) 
  enigmaEncode plain (SteckeredEnigma lr mr rr reflector origOff sboard) 
    -- check the length of Steckerboard is valid 
    = if (length sboard < 11) then steckeredChar else error"Invalid Steckerboard"    
    where
      encodedChar = simpleEnigmaEncode (reflectorPair sboard plain) (SimpleEnigma lr mr rr reflector origOff)
      steckeredChar = reflectorPair sboard encodedChar
  
  -- This function takes in a message and Enigma and return the corresponding encrypted message
  -- Assumptions & Validations
  -- It is assumed that user will supplied upper case characters for encryption. 
  -- There are validation for the rotors, the length of reflector and offsets.    
  enigmaEncodeMessage :: String -> Enigma -> String
  enigmaEncodeMessage [] _ = []
  enigmaEncodeMessage (x:xs) (SimpleEnigma lr mr rr reflector origOff) 
    = enigmaEncode x (SimpleEnigma lr mr rr reflector origOff)
      :(enigmaEncodeMessage xs (SimpleEnigma lr mr rr reflector (advanceOffsets origOff)))    
  enigmaEncodeMessage (x:xs) (SteckeredEnigma lr mr rr reflector origOff sboard) 
    = enigmaEncode x (SteckeredEnigma lr mr rr reflector origOff sboard)
      :(enigmaEncodeMessage xs (SteckeredEnigma lr mr rr reflector (advanceOffsets origOff) sboard))    
 
  -- This function find the longest Menu from the provided Crib
  -- Assumption: It is assumed that user will supply the Crib in either lower case or upper case characters, not a mixture of it    
  -- I made use of the n-ary tree to build a tree of different path that the root node can reach 
  -- and find the longet one amoung all of the trees I build
  -- So, for example, if the crib is of the length 3, I will have three n-ary trees with the root node,0,1,2 representively.
  -- After building the trees, I will find the longest menu of each tree, compare the three menus of the trees 
  -- and return the longest one among all of them.    
  longestMenu :: Crib -> Menu
  longestMenu (plain,cipher) = bestRoute
    where
      lenCrib =  if (length plain == length cipher) then (length plain) else error"Invalid Crib" -- validate the Crib 
      allMatches = createMatches (plain,cipher) [0..(lenCrib-1)]
      allmaxMenu = buildAllTrees allMatches [0..(lenCrib-1)] 
      allLen = lengthOfMenus allmaxMenu
      maxLen = maximum allLen
      indexMaxLen = findIndices (==maxLen) allLen
      bestRoute = allmaxMenu!!(indexMaxLen!!0)

  {-------------------------------------------------------Helper Functions of Assignment 2----------------------------------------------------------}
  -- Take a list and covert it into nodes
  -- Example: Input: convertList [1,2,3]  Output: [Node 1 [],Node 2 [],Node 3 []]
  convertList :: [Int] -> [Tree a]
  convertList [] = []
  convertList (poss1:possRest) = (Node poss1 []):convertList possRest

  -- There are validation on crib before calling this function. 
  -- It is assumed that user supplies the integer list in correct format.
  -- Takes in a Crib and a list that contains 0 to the length of the Crib-1 and 
  -- return a list of all one-step link of a letter in plain to its next reachable position in plain 
  --  in the format (index of the start in plain,index of the next step in plain)
  -- Example: createMatches ("ABCDE","ZABCD") [0..4]  -> [(1,0),(2,1),(3,2),(4,3)]
  createMatches :: Crib -> [Int] -> [Match]  
  createMatches _ []  = []
  createMatches (plain,cipher) (x:xs) = returnList ++ createMatches (plain,cipher) xs 
    where 
      target = plain!!x
      possWay = findIndices (== target) cipher
      returnList = createCombin x possWay 
  
  -- Take in an integer and a list and generate tuples in the pattern (given integer,item in the given list)
  -- Example: Input:createCombin 1 [1,2,3]  Output: [(1,1),(2,1),(3,1)]
  createCombin :: Int -> [Int] -> [Match]    
  createCombin _ []  = []
  createCombin first (p:ps)  = [(p,first)] ++ createCombin first ps 

  -- Take a list of matches and a list contain the unwanted matches and return the updated list with no unwanted matches  
  -- Example: Input: removeListElem [(1,2),(2,3),(3,4)] [(1,2),(2,3)]  Output: [(3,4)]
  removeListElem :: [Match] -> [Match] -> [Match]
  removeListElem updatedMatches [] = updatedMatches 
  removeListElem allMatches (item1:restItems) = removeListElem (removeElem allMatches item1) restItems   

  -- Take a list of matches and a single match and return the list without that match 
  -- Example: Input: removeElem [(1,2),(2,3),(3,4)] (1,2)     Output: [(2,3),(3,4)]
  removeElem :: [Match] -> Match -> [Match]
  removeElem [] _ = []
  removeElem (match1:restMatches) itemRemove 
    | (match1 == itemRemove) = removeElem restMatches itemRemove
    | otherwise              = [match1] ++ removeElem restMatches itemRemove 
  
  -- Take a list of matches and an integer and delete the matches that the second integer equals the given integer  
  -- Example: Input: removeIfSecondIs [(1,2),(2,3),(3,4)] 4   Output:[(1,2),(2,3)]  
  removeIfSecondIs :: [Match] -> Int -> [Match]
  removeIfSecondIs [] _ = []
  removeIfSecondIs ((a0,a1):as) n 
    | (a1 == n) = removeIfSecondIs as n
    | otherwise = (a0,a1):removeIfSecondIs as n

  -- Take a list of matches and an integer and return a list of all possible next step of that integer
  -- Example: Input: findNext [(0,1),(1,2),(2,3),(3,4),(1,3),(1,4)] 1    Output: [2,3,4]
  findNext :: [Match] -> Int -> [Int]
  findNext [] _ = []
  findNext (x:xs) target
    | ((fst x) == target) = [snd x] ++ findNext xs target
    | otherwise           = findNext xs target
  
  -- This function does not work on trees that have anything more than a root node  
  -- Take a list of matches and a tree with root only and return the full tree based on that root    
  -- Example: buildTree [(1,0),(2,1),(3,2),(4,3)] (Node 1 [])   Output: Node 1 [Node 0 []]
  -- Note: [(1,0),(2,1),(3,2),(4,3)] is the output of createMatches ("ABCDE","ZABCD") [0..4]
  buildTree :: [Match] -> Tree a -> Tree a
  buildTree _ Empty = Empty
  buildTree allMatches (Node a [])
    --map the recursive call of the function on every possible next step with the matches list that does not contained used matches 
    = Node a (map (\x -> buildTree (removeIfSecondIs (removeListElem allMatches (createCombin a [(getRoot x)])) a ) x) 
      (convertList (findNext allMatches a))) -- find the list of next possible step and turn them into nodes

  -- It is assumed that user supplies the integer list in correct format.    
  -- Take a list of matches and a list containing all possible roots ([0..(length of crib -1)]), build the trees and
  --  return a list containing the longest menu for each root  
  -- Example: Input:buildAllTrees [(1,0),(2,1),(3,2),(4,3)] [0..4]  Output:[[0],[1,0],[2,1,0],[3,2,1,0],[4,3,2,1,0]]
  -- Note: [(1,0),(2,1),(3,2),(4,3)] is the output of createMatches ("ABCDE","ZABCD") [0..4]
  buildAllTrees :: [Match]-> [Int] -> [Menu]
  buildAllTrees _ [] = []
  buildAllTrees allMatches (root1:restRoots) = [maxRoute (currentTree)] ++ buildAllTrees allMatches restRoots
    where
      currentTree = buildTree allMatches (Node root1 [])
  
  -- Take a list of routes and return a list containing the corresponding length of the route 
  -- Example: Input:lengthOfMenus [[0],[1,0],[2,1,0],[3,2,1,0],[4,3,2,1,0]]  Output:[1,2,3,4,5] 
  lengthOfMenus :: [Menu] -> [Int]
  lengthOfMenus [] = []
  lengthOfMenus (menu1:restMenus) = [length menu1]++lengthOfMenus restMenus         

  -- It is assumed that user give a tree with root node only
  -- Take a Node and return the integer value of it
  -- Exmaple: Input: getRoot (Node 4 [])   Output:4
  getRoot :: Tree a -> Int
  getRoot (Node a _) = a

  -- Take a tree and return the maximum height of it
  -- Example: Input: maxHeight (Node 2 [Node 1 [Node 0 []],Node 5[Node 7 [Node 8[]]]]) Output: 4
  maxHeight :: Tree a -> Int
  maxHeight Empty = 0
  maxHeight (Node a []) = 1
  maxHeight (Node a subtree) = 1 + (maximum (map maxHeight subtree))

  -- Take a list of trees and return the longest tree out of the list
  -- Example: Input: longestSubTree [Node 1 [Node 0 []],Node 5[Node 7 [Node 8[]]]] Output: Node 5 [Node 7 [Node 8 []]]    
  longestSubTree :: [Tree a] -> Tree a
  longestSubTree [] = Empty
  longestSubTree [treeA] = treeA
  longestSubTree (treeA:treeB:restTrees) 
    | (maxHeight treeA >= maxHeight treeB) = longestSubTree (treeA:restTrees)
    | (maxHeight treeA < maxHeight treeB)  = longestSubTree (treeB:restTrees)

  -- Take a tree and return one of the route that the length is equal to the maximum length
  -- Input:maxRoute (Node 2 [Node 1 [Node 0 []],Node 5[Node 7 [Node 8[]]]])   Output:[2,5,7,8]        
  maxRoute :: Tree a -> [Int]
  maxRoute Empty = []
  maxRoute (Node x subtrees) = x:maxRoute (longestSubTree subtrees)
  
  -- It is assumed that user supplied the upper case character for encryption and there are no repeat for the reflectors.
  -- There are validtion on rotors, reflector length and offsets.
  -- A function that do the Encoding of a Simple Enigma
  -- Example: Input:simpleEnigmaEncode 'A' (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25)) Output: 'N'
  simpleEnigmaEncode :: Char -> Enigma -> Char
  simpleEnigmaEncode plain (SimpleEnigma lr mr rr reflector origOff) = rREncoded
    where  
      (lAdvancedOff,mAdvancedOff,rAdvancedOff) = advanceOffsets origOff --Advanced Offsets
      rencoded = if (validateCipher rr) then (encode2 rr rAdvancedOff plain) else error "Invalid Cipher"
      mencoded = if (validateCipher mr) then (encode2 mr mAdvancedOff rencoded) else error "Invalid Cipher"
      lencoded = if (validateCipher lr) then (encode2 lr lAdvancedOff mencoded) else error "Invalid Cipher"
      reflect = if (length reflector == 13) then (reflectorPair reflector lencoded) else error"Invalid Reflector"
      lREncoded = reverseEncode2 lr lAdvancedOff reflect
      mREncoded = reverseEncode2 mr mAdvancedOff lREncoded
      rREncoded = reverseEncode2 rr rAdvancedOff mREncoded
  
  -- A function that take a set of offset and advance it by 1
  -- Example: Input: advanceOffsets (25,25,25) Output: (0,0,0)
  advanceOffsets :: Offsets -> Offsets
  advanceOffsets (loff,moff,roff)
    -- Check if the Offsets valid 
    | (roff > 25) || (moff>25) || (loff>25) || (roff < 0) || (moff < 0 ) || (loff < 0)   = error"Invalid Offsets"
    -- Check if there is any need to advance other rotors other than the right rotor
    | (roff + 1) == 26 = if (moff + 1) == 26 then (if (loff + 1) == 26 then (0,0,0) else ((loff+1),0,0)) else (loff,(moff+1),0)
    | otherwise        = (loff,moff,roff+1)    
  
  -- Take a Steckerboard and Character and exchange it according to the given Steckerboard  
  -- Input: reflectorPair [('A','V'),('U','W')] 'A' Output: 'V' 
  reflectorPair :: Steckerboard -> Char -> Char 
  reflectorPair [] x = x
  reflectorPair ((plain,guess):restList) x 
    | (x == guess) = plain
    | (x == plain) = guess
    | otherwise = reflectorPair restList x
  
  -- It is assumed that check on rotor is done before calling the fucntion
  -- Take a character,a rotor and an offset and return the encoded character based on the given rotor and offset
  -- Example: Input:encode2 rotor1 1 'A'     Output:'J'
  encode2 :: Cipher -> Int -> Char -> Char
  encode2 currentRotor offset character = thirdChar   
    where
      firstChar = encode alphabet offset character
      secondChar = encode currentRotor 0 firstChar
      thirdChar = encode alphabet (-offset) secondChar

  -- It is assumed that check on rotor is done before calling the fucntion
  -- Take a character,a rotor and an offset and return the reverse-encoded character based on the given rotor and offset
  -- Example: Input: reverseEncode2 rotor1 1 'W' Output: 'P'
  reverseEncode2 :: Cipher -> Int -> Char -> Char
  reverseEncode2 currentRotor offset character = thirdChar
    where
      firstChar = encode alphabet offset character
      secondChar = reverseEncode currentRotor 0 firstChar
      thirdChar = encode alphabet (-offset) secondChar

  {-------------------------------------------------Assignment 1 : Substitution Ciphers Code -------------------------------------------------}
  -- This function validate a cipher – which must contain each letter once and once only
  validateCipher :: Cipher -> Bool 
  validateCipher cipher
    | (length cipher == 26) = unique cipher
    | otherwise = False
    where 
      {- A function used to test if there is any equal letter in the input Cipher and return true if there are no 
         equal letters -}
      -- Update: check if the character is a upper case character or not 
      unique :: Cipher -> Bool
      unique [] = True
      unique (x:xs)
          | elem x xs = False
          | isUpper x = True 
          | otherwise = unique(xs)

  -- This function takes a cipher, an offset and a character, 
  -- and returns the corresponding encoded character
  encode :: Cipher -> Int -> Char -> Char
  encode oCipher off char = encodeChar (shiftCipher oCipher off) char
    where
      -- A function that take a shifted cipher and a character as input and return the encoded character 
      encodeChar :: Cipher -> Char -> Char
      encodeChar sCipher orig 
        | orig == ' ' = ' '
        | otherwise   = sCipher!!(alphaPos orig)
  
  --This function takes a cipher, an offset, and a message, uses encode to encode 
  -- and return the complete encoded message
  encodeMessage :: Cipher -> Int -> String -> String
  encodeMessage oCipher off [] = []
  encodeMessage oCipher off (x:xs) = (encode oCipher off x) : (encodeMessage oCipher off xs) 
  
  -- This function takes a cipher, an offset and an encoded character, and returns the plain character
  reverseEncode :: Cipher -> Int -> Char -> Char
  reverseEncode oCipher off char = decodeChar (shiftCipher oCipher off) char
    where
      -- A function that take a shifted cipher and a encoded character as input and return the decoded character       
      decodeChar :: Cipher -> Char -> Char
      decodeChar sCipher encrypt 
        | encrypt == ' ' = ' ' 
        | otherwise      = ['A'..'Z']!!((findIndices (==encrypt) sCipher)!!0)

  -- This function takes a cipher, an offset and an encoded message 
  -- and return the plain text message
  reverseEncodeMessage :: Cipher -> Int -> String -> String
  reverseEncodeMessage oCipher off [] = []
  reverseEncodeMessage oCipher off (x:xs) = (reverseEncode oCipher off x) : (reverseEncodeMessage oCipher off xs) 

  -- This function takes a message, 
  -- and returns the percentage of each letter occurring in this message as a list of (Char, Int) tuples
  letterStats :: String -> [(Char,Int)]
  letterStats xs = mergesort (\a b -> (snd a) >= (snd b)) (letterStats' xs (length xs)) 
    where
      {- a function that take a string and the length of the string and retrun the list of the percentage of a each character 
         in a the given String -}
      letterStats' :: String -> Int -> [(Char,Int)]
      letterStats' [] n = []
      letterStats' (x:xs) n 
        | notElem x xs = (x, (percent numX n)) : (letterStats' xs n)
        | otherwise = (x, (percent numX n)) : (letterStats' ys n)
          where 
          numX = length (filter (== x) (x:xs))
          ys = filter (/=x) xs
   
  -- This function takes a list of guesses for letters in the message
  -- and returns the message with these guesses filled in, in lower case 
  partialDecode :: [(Char, Char)] -> String -> String 
  partialDecode [] decodedMessage = decodedMessage
  partialDecode ((plain,guess):restList) origMessage = partialDecode restList (findReplace (plain,guess) origMessage)
    where
      -- A function that take an encoded letter guess and the encoded string as input and return the string with replaced character
      findReplace :: (Char, Char) -> String -> String
      findReplace _ [] = []
      findReplace (plain,guess) (x:xs) 
        | x == guess     = (toLower plain) : (findReplace (plain,guess) xs)
        | otherwise      = x : (findReplace (plain,guess) xs)

  ----------------------------------------------------------------------------------------------------------------------------------------
  -- Helper functions  -------------------------------------------------------------------------------------------------------------------
  -- A function used to shift the cipher with the offset  
  shiftCipher :: Cipher -> Int -> Cipher
  shiftCipher orig off 
    | validateCipher(orig) = prefix ++ rest
    | otherwise = error "Invalid Cipher"
    where (rest,prefix) = splitAt ((off+26)`mod` 26) orig