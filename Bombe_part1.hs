{-  Name: Hayley Wing Yin Kwok
    COM 2108 Functional Programming Assignment 3 -- Part 1 Bombe with fixed rotors:  rotor1 rotor2 rotor3-}

module Bombe where
    import AssignmentHelp
    import Enigma
    import Data.Char
    import Data.List
    import Data.Maybe
    
    type SteckerPair = (Char,Char)
    
    -- This function takes in a Crib (original message, encrypted message) and returns valid steckerboard and offset if a solution exists
    -- It will returns nothing if no solution is found
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
    breakEnigma (plain,cipher) 
      = breakEA (plain,cipher) menu [(firstMenuChar,'A')] (0,0,0)   
      where
        menu = longestMenu (plain,cipher)
        firstMenuChar = plain!!(menu!!0)
    --Testing: 
    -- breakEnigma ("AFJEQTMC","FJEQTMCF")
    --   return Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')])
    -- breakEnigma ("COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE","QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW")
    --   return Just ((0,0,0),[('Q','C'),('W','I'),('X','A'),('U','E'),('H','S'),('G','T'),('R','M')])
        

    -- This function checks all possible steckerboards for all possible offsets.
    -- It will returns nothing if no solution is found
    -- Assumption: It is assume the offset at the first call of this function is (0,0,0)
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
    breakEA crib menu stecker origOffset
      | (nextOffset == (0,0,0)) = Nothing
      | (nothingP successOffset) = breakEA crib menu stecker nextOffset  
      | not(nothingP successOffset) = Just (origOffset,(AssignmentHelp.fromMaybe successOffset))
      | otherwise = Nothing
      where
        successOffset = findStecker crib menu stecker origOffset
        nextOffset = advanceOffsets origOffset
    -- Testing
    -- Success Case: 
    -- breakEA ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0)
    --  return Just ((0,0,0),[('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')])
    -- breakEA ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) 
    --   return Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')])
    -- Failing Case (no solution to the given Crib & Menu)
    --   (mean the system should have tried all combinations of the offset and initial pair of the steckerboard):
    -- breakEA ("ZGXWAUTS","AAAAAAAA") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) return Nothing
        
    -- This function checks all possible steckerboards for a given offset
    -- It will returns nothing if no solution is found
    -- Assumption: Assume that the steckerboard is [(initial,'A')] (it cannot be other character for the second element)
    findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    findStecker crib menu [(initial,altering)] offset
      | (alteringIndex+1) > 25 = Nothing 
      | (nothingP successStecker) = findStecker crib menu [(initial,nextTry)] offset
      | not(nothingP successStecker) = Just (removeEqualStecker (AssignmentHelp.fromMaybe successStecker))
      | otherwise = Nothing
      where
        successStecker = followMenu crib menu [(initial,altering)] offset
        alteringIndex = (elemIndices altering alphabet)!!0
        nextTry = alphabet!!(alteringIndex+1)
    --Testing  
    --Success Case:
    --  findStecker ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) 
    --   return Just Just [('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')]
    --  findStecker ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0)
    --   return Just [('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')]
    --Failing Case (Wrong offset):
    --  findStecker ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,1) return Nothing
    --  findStecker ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (25,25,25) return Nothing
    --Failing Case (no solution to the given Crib):
    --  findStecker ("ZGXWAUTS","AAAAAAAA") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) return Nothing

    -- With a given offset, menu and initial steckerboard, 
    -- this function recursively tries to add new pair to the existing steckerboard
    -- It will returns nothing if no solution is found
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    followMenu _ [] existingStecker _ = Just existingStecker
    followMenu (plain,cipher) (m:ms) existingStecker offset
      |(encodedChar == cipherChar) = followMenu (plain,cipher) ms ((encodedChar,cipherChar):existingStecker) offset
      |not(nothingP newStecker) = followMenu (plain,cipher) ms (AssignmentHelp.fromMaybe newStecker) offset
      | otherwise = Nothing      
      where  
        plainChar = plain!!m 
        steckerChar = if (checkStecker existingStecker plainChar) then (reflectorPair existingStecker plainChar) else plainChar
        correspondOffset = advanceMoreOffset offset m
        encodedChar = enigmaEncode steckerChar (SimpleEnigma rotor1 rotor2 rotor3 reflectorB correspondOffset)
        cipherChar = cipher!!m
        newStecker = steckerAdd (cipherChar,encodedChar) existingStecker
    --Testing
    --Success Case:    
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) 
    --  return Just [('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('J','J'),('B','F'),('A','A')]
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','S')] (0,0,0) 
    --  return Just [('G','G'),('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T'),('S','S')]
    --Failing Case Testing (wrong Start Pair) :
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) return Nothing
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','Z')] (0,0,0) return Nothing
    --Failing Case Testing (wrong offset) : 
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,1) return Nothing
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','S')] (25,25,8) return Nothing 
      
    -- This function will add the given SteckPair to the Steckerboard if it is a valid input to the existing Steckerboard
    -- If the given SteckPair is invalid, the function will return nothing
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (x,y) existingStecker
      | not(checkStecker existingStecker x) && not(checkStecker existingStecker y) = Just ((y,x):existingStecker)
      | (equalStecker (x,y) existingStecker) = Just existingStecker
      | otherwise = Nothing
    -- Testing
    -- Success Case:
    --  steckerAdd ('O','U') [('A','H')] return Just [('U','O'),('A','H')]
    -- Border Case Testing (Same element to the existing pairs in the SteckerBoard ):
    --  steckerAdd ('A','H') [('A','H')] return Just [('A','H')]
    --  steckerAdd ('A','H') [('H','A')] return Just [('H','A')]
    -- Nothing Case Testing (Conflict pair to the existing pairs in the SteckerBoard):
    --  steckerAdd ('H','H') [('A','H')] return Nothing
    --  steckerAdd ('A','A') [('A','H')] return Nothing    



    {----------------------------------------------------Helper Functions-------------------------------------}
    -- Return False if the given character does not exist in the Steckerboard 
    -- False example: checkStecker [('A','U'),('I','P')] 'O' return False
    -- True Example:  checkStecker [('A','U'),('I','P')] 'I' return True
    checkStecker :: Steckerboard -> Char -> Bool 
    checkStecker [] _ = False
    checkStecker ((plain,guess):restList) x 
      | (x == guess) || (x == plain) = True || checkStecker restList x
      | otherwise = False || checkStecker restList x
    
    -- Return True if the given SteckerPair exist in the Steckerboard 
    -- False example: equalStecker ('A','A') [('H','A'),('K','L')] return False
    -- True Example:  equalStecker ('A','H') [('H','A'),('K','L')] return True  
    equalStecker ::  SteckerPair -> Steckerboard -> Bool
    equalStecker _ [] = False
    equalStecker (x,y) ((plain,guess):restList)
      | ((x == plain) && (y == guess)) || ((x == guess) && (y == plain)) = True || equalStecker (x,y) restList
      | otherwise = False || equalStecker (x,y) restList
    
    -- Remove the SteckerPair from the Steckerboard if both elements in the SteckerPair are the same  
    --removeEqualStecker [('A','A'),('I','I'),('O','P')] return [('O','P')] 
    -- Nothing to delete testing: removeEqualStecker [('O','P')] return [('O','P')] 
    removeEqualStecker :: Steckerboard -> Steckerboard
    removeEqualStecker [] = []
    removeEqualStecker ((x,y):rest)
      | (x==y) = removeEqualStecker rest
      | otherwise = (x,y):(removeEqualStecker rest)

    -- Advance the offset by the amount of the given number
    -- advanceMoreOffset (0,0,0) 9 return (0,0,9)
    -- Border Case testing: advanceMoreOffset (25,25,25) 9  return (0,0,8)
    advanceMoreOffset :: Offsets -> Int -> Offsets
    advanceMoreOffset offset 0 = offset
    advanceMoreOffset offset n = advanceMoreOffset (advanceOffsets offset) (n-1)