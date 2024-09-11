module Ants where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 5529791
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.

-- Outside resources
-- To assist with Ex 5 I used The Nature of Code by Daniel Shiffman, specifically
-- Chapter 7 (Cellular Automata). This resource helped me to understand how there
-- can be 256 possible rules, and gave me the idea of using binary to implement
-- the allRules function.
-- This resource can be found at: https://natureofcode.com/cellular-automata/

--------------------------------------------------------------------------------

import Ants.Types
import Hatch

import Data.Set (Set)
import qualified Data.Set as Set




{-| 
  Ex. 1: Implement a Show instance for LineState.

  (The pipe character and full block character are | and █)

  [JUSTIFY]

  This function was implemented using a list comprehension, which generates a list of characters. The type of each character 
  is dependent on the contents of the function's input 'list' (which is of type LineState). 
  
  For every element in 'list', if it is the CellState 'On', then the character '█' will be added to the new list. If it is 
  the CellState 'Off', then the character ' ' is added instead. This logic was achieved using an if...else... statement - I
  used this approach because I believe it is the most readable, and I did not want to overcomplicate this simple function.

  I used a list comprehension for this function because I knew that a list of characters is just a string. This made it 
  easy to concatenate the CellState representation between pipes ("|"), thus obtaining the intended result.
-}

instance Show LineState where
  show :: LineState -> String
  show (LS list) = "|" ++ [ if x == On then '█' else ' ' | x <- list ] ++ "|"



{-|
  Ex. 2: Implement ruleX, which turns a cell on if it has exactly one neighbour 
  which was on in the previous step. The three arguments are the states of the 
  left neighbour, current cell, and right neighbour respectively.

  [ JUSTIFY ]

  This function takes three CellStates (l, c and r) where l and r represent the cells on the left and right of c respectfully. 
  The function then uses XOR between the truth values of whether l and r are 'On'. I used XOR because it is a more efficient 
  way of achieving the desired result, since the logical statement only returns True when just one of l or r are 'On' - not 
  when both are 'On' or both are 'Off'. 

  The desired output is then produced using an if...else... statement, which returns the CellState 'On' if just one of l or r 
  are 'On' - otherwise, 'Off' is returned. I used an if...else... statement because I believe it is the most readable.
  However, I could have used another approach, such as using guards:

  ruleX :: CellState -> CellState -> CellState -> CellState
  ruleX l c r 
  | (l == On) /= (r == On) = On
  | otherwise = Off

  But I believe my approach is better as it is just as readable and uses fewer lines of code.
-}

ruleX :: CellState -> CellState -> CellState -> CellState
ruleX l c r = if (l == On) /= (r == On) then On else Off



{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state, 
  applies the rule to each cell in the state and returns the updated state.

  [JUSTIFY]

  The apply rule function uses the newList function. I placed the newList function under a where clause to make it clear that it
  is being used in applyRule.

  The purpose of the newList function is to obtain function arguments to apply a rule to each cell, for a group of three
  cellState's (left, middle, right). For example:
  
  In order to apply a rule to the left cell in a group of three cells, the following arguments are required:
  * Off   * The left cell  * The middle cell

  To apply the rule to the middle cell, the arguments would be: * The left cell  * The middle cell  * The right cell
  To apply the rule to the right cell, the arguments would be: * The middle cell  * The right cell  * Off

  * The newList function uses the 'list' input to create two other lists. The first list is leftList, which is the original
  'list' with a padding at the start of 'Off'. This represents the arguments required to apply the rule to the leftmost cell.
  * The second list is rightList, which removes the first element of 'list', and pads with 'Off' at the end of the list.
  This represents the arguments to apply the rule to the rightmost cell. 
  * The arguments to apply the rule to the middle cell are the same as 'list'.

  It is necessary to use drop to remove the first element of list to obtain rightList, since zip3 uses the first three elements 
  of each list input. However, it is not requried to remove the last element in leftList since zip3 does not use this value. 

  Next, the zip3 function is applied to leftList, 'list' and rightList, to return a new list containing three triples. 
  The first triple will contain the first cellState in leftList, the first cellState in 'list' and the first cellState in rightList.
  The second triple will contain the second cellState in leftList, the second cellState in 'list' and the second cellState in rightList.
  The same logic applies for the third triple. 
  
  This function results in a new list of three triples, where (because of the nature of binary shifting) each triple is a group 
  of arguments to apply the rule to that particular cell. 
  Next, using a list comprehension, the rule is applied to all three triples. This results in a list (which is then converted 
  into a LineState) where every element is a CellState where the rule has been applied to it. This is the original line state
  argument with the rule having been applied to it.


-}


applyRule :: (CellState -> CellState -> CellState -> CellState)
          -> LineState
          -> LineState
applyRule rule lineState = LS ([rule x y z | (x, y, z) <- newList lineState])
  where
    newList :: LineState -> [(CellState, CellState, CellState)]
    newList (LS list) = zip3 leftList list rightList
      where leftList = Off : list 
            rightList = drop 1 $ list ++ [Off]



{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a 
  starting configuration, and returns the number of the iteration at 
  which the automaton first revisits a state.

  [JUSTIFY]

  I made the decision to implement the main body of the loopedAt function in a seperate function called loopedAtBody. This is so that
  I could pass in an empty list [] (which would be used to store lineStates after each time the rule was applied) and 'count', which
  represents the number of iterations before the automaton first revisits a state. loopedAtBody is within a where clause, to make
  it clear that it is being used in loopedAt.

  This is a recursive function, where the base case is met only when the next time the rule is going to be applied to the lineState,
  the result is already found in the temporary list. This is detected using 'elem'. In this case, the number of iterations thus 
  far is returned.

  When the base case has not been met, there is a recursive call to loopedAtBody. Now, the line state passed into it is the previous
  line state except the rule has been applied again. The temporary list has the current line state appended to it. Also, count
  is incremented.

  I used a where clause below loopedAtBody to make my code more readable, since 'nextLineState' is used multiple times.

  In the actual loopedAt function, a call is made to loopedAtBody, with the following arguments:
  1) The rule to be applied  2) The line state  3) A temporary list to store line states  4) Count

-}


loopedAt :: (CellState -> CellState -> CellState -> CellState)
  -> LineState
  -> Int
loopedAt rule lineState = loopedAtBody rule lineState [] 1
  where
    loopedAtBody :: (CellState -> CellState -> CellState -> CellState)
      -> LineState
      -> [LineState]
      -> Int
      -> Int
    loopedAtBody rule lineState tempList count
      | nextLineState `elem` tempList = count
      | otherwise = loopedAtBody rule nextLineState (nextLineState:tempList) (count + 1)
      where
        nextLineState = applyRule rule lineState



{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  [JUSTIFY]

  I began this exercise by trying to understand how there can be 256 sets of rules. I eventually understood the following:
  * There are 8 possible combinations of groups of three cell states (since 2^3 = 8).
  * Each group of cells is mapped to either 'On' or 'Off', depending on the rule that is being applied.
  * Therefore, a rule can be defined by a unique group of '8 bits' (or a sequence of 8 cell states)
  * Since 2^8 = 256, there are 256 possible rules.

  I decided to implement the allRules function using the principles of binary. 
  
  Firstly, I made a decimalToBinary function. This uses recursion to repeatedly divide an integer by 2 (using the integer division
  function div), and appends the remainder (obtained using mod) to a list. The base case is met once the an attempt to divide
  0 by 2 is made - in which case, the list is returned. Because the remainders are added recursively to the list, they are added
  in reverse order as intended.

  Next, I made padWithZeros, which pads a list of binary bits with zeros at the start, until the number is the required number of
  bits. This function is also recursive - the base case is met once the list is the required number of bits (this is checked using 
  the length function). The recursive call involves calling the function again, except the list that is passed is the previous list
  with a 0 appended to the start.

  To convert this list of binary bits into a LineState, I made the toLineState function. This returns a LineState constructed
  using list comprehension based on the list input - for each element in the list, if it is a 1, then 'On' is added to the return
  list. Otherwise, if it is a 0, then 'Off' is added.

  The function eighBitCombos is used to access a list of every unique LineState with length 8. This is achieved using list 
  comprehension. Every decimal number between 0 and 255 is considered, converted into binary (using decimalToBinary), padded 
  with zeros (using padWithZeros), and then converted into a lineState (using toLineState).

  The function buildRuleFromBinary takes four arguments:
  1) A sequence of 8 bits  2) The left cell state  3) The middle cell state  4) the right cell state
  
  The function considers every 3 bit combination of On's and Off's, and maps it to an index of the 8 bit sequence argument.
  For example:
    Suppose: Sequence = Off, On, Off, Off, Off, On, On, On
    Off, Off, Off -> Sequence[0] = Off
    Off, Off, On -> Sequence[1] = On
    Off, On, Off -> Sequence[2] = Off
    ... etc ...

  This is achieved using 8 guards, where each guard queries whether the left, middle and right arguments to the function
  are a particular sequence of 3 bits. An index value (unique to that combination) of the 8 bit sequence is then returned.
  I could have used another approach for this, by generating a list of line states of length 3, and mapping each particular 
  combination (an index of that list) to a bit in the given eight bit sequence. However, I think that my approach is better because 
  its easier to visualise the logic behind the function, even though it requires more code.

  Finally, the allRules function uses list comprehension to partially apply buildRuleFromBinary to elements of eightBitCombos.
  This results in a list (of length 256) of functions which have been given a unique 8 bit sequence of On's and Off's. In order to 
  apply each of these rules later, three cell states (for the left middle and right cell) need to be provided.

  To organise my code I used two where clauses. The first is to show that buildRuleFromBinary and eightBitCombos is used
  within allRules. The second is to show that toLineState, padWithZeros and decimalToBinary are used in eightBitCombos.
  I presented my code in this way so it is easier to follow the relationships between functions.

-}



allRules :: [ CellState -> CellState -> CellState -> CellState ]
allRules = [buildRuleFromBinary x | x <- eightBitCombos]
  where
    buildRuleFromBinary :: LineState -> CellState -> CellState -> CellState -> CellState
    buildRuleFromBinary (LS bitSequence) left middle right
      | (left == Off) && (middle == Off) && (right == Off) = bitSequence !! 0
      | (left == Off) && (middle == Off) && (right == On) = bitSequence !! 1
      | (left == Off) && (middle == On) && (right == Off) = bitSequence !! 2
      | (left == Off) && (middle == On) && (right == On) = bitSequence !! 3
      | (left == On) && (middle == Off) && (right == Off) = bitSequence !! 4
      | (left == On) && (middle == Off) && (right == On) = bitSequence !! 5
      | (left == On) && (middle == On) && (right == Off) = bitSequence !! 6
      | (left == On) && (middle == On) && (right == On) = bitSequence !! 7

    eightBitCombos :: [LineState]
    eightBitCombos = [toLineState $ padWithZeros (decimalToBinary x []) 8 | x <- [0..255]]

      where
        toLineState :: [Int] -> LineState
        toLineState list = LS [if x == 1 then On else Off | x <- list]

        padWithZeros :: [Int] -> Int -> [Int]
        padWithZeros list x
          | length list == x = list
          | otherwise = padWithZeros (0:list) x

        decimalToBinary :: Int -> [Int] -> [Int]
        decimalToBinary x list
          | x == 0 = list
          | otherwise = decimalToBinary (div x 2) $ (mod x 2):list
  


{-|
  Ex. 6: Implement initialState, which returns the initial 
  configuration of Langton's Ant.

-}
initialState :: AntState
initialState = AS West (0, 0) Set.empty



{-|
  Ex. 7: Define the functions leftOf and rightOf, which 
  return the direction to the left and right of the given direction, 
  respectively. Follow the additional constraints given in the 
  specification, and answer the written question here.

  [JUSTIFY]

  I defined rightOf in terms of top-level pattern matching, and the method I chose to use for leftOf was a case...of... statement.

  I prefer the case...of... statement because I think that it's easier to read since the function name is not repeated. Also,
  I think that it's clearer that, for example, the input North leads to an output West because of the symbol ->. 
  It's easy to subconsciously associate the = symbol with equality even though it is specifying the function's result.

-}

leftOf :: Direction -> Direction
leftOf x = case x of
  North -> West
  East -> North
  South -> East
  West -> South

rightOf :: Direction -> Direction
rightOf North = East
rightOf East = South
rightOf South = West
rightOf West = North



{-|
  Ex. 8: Implement the step function, which takes the ant 
  state and applies the logic given in the specification 
  to return the new ant state.

  [JUSTIFY]

  In order to implement this function, I made the nextSquare function. This uses a case...of... statement to output the co-ordinate
  that the ant should move to, based on the current co-ordinate and the direction of movement. I could have also used
  pattern matching or guards, but I presented the function this way as I perceieve it to be the most elegant approach, 
  as you can clearly see the directions being mapped to a specific co-ordinate.

  I also made the currentSquareBlack function, which outputs the truth value of whether the square the ant is currently occupying
  is black. This is achieved using the member function from data.set, to determine whether the current co-ordinates can be found
  in the set blackSquares.

  In the actual step function, I used an if...else... statement to determine the sequence of logic - I used this approach (rather
  than pattern matching for example) since there are only two possible outcomes that are the inverse of eachother. 

  When the method currentSquareBlack returns true, the ant is made to face left. This is achieved by returning an AntState where 
  the direction is the previous direction with the function leftOf applied to it. In addition, the current position of this 
  new ant state is obtained using the nextSquare function, and passing the previous AntState, and the new direction. Finally, 
  the previous current square is deleted from the blackSquares set (using the data.set function delete) and this new set is 
  passed into the new AntState.

  Almost identical logic applies for when currentSquareBlack returns false. However, all calls to leftOf are replaced with calls to 
  rightOf. In addition, instead of removing the previous current cell from blackSquares, it is inserted into blackSquares using the 
  data.set function insert.

  To make my code more elegant, I used a where clause to contain the constants facingLeft, facingRight (calls to the functions
  leftOf and rightOf respectfully) and previousState (the AntState in its deconstructed form). This is because they are 
  referenced multiple times in the main body of the  function, and using a where clause makes the main body of the function more 
  readable. I also added functions that are used in the main body of step (nextSquare and currentSquareBlack) into the where 
  clause in order to make it clear where they are being used.

-}



step :: AntState -> AntState
step (AS direction current blackSquares) = 
  if currentSquareBlack (AS direction current blackSquares) then
    AS facingLeft (nextSquare previousState facingLeft) (Set.delete current blackSquares)
  else
    AS facingRight (nextSquare previousState facingRight) (Set.insert current blackSquares)
  where
    facingLeft = leftOf direction
    facingRight = rightOf direction
    previousState = (AS direction current blackSquares)
    
    nextSquare :: AntState -> Direction -> (Int, Int)
    nextSquare (AS direction (x, y) blackSquares) newDirection = case newDirection of
      North -> (x, y + 1)
      East -> (x + 1, y)
      South -> (x, y - 1)
      West ->  (x - 1, y)

    currentSquareBlack :: AntState -> Bool
    currentSquareBlack (AS direction current blackSquares) = Set.member current blackSquares


{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by 
  implementing the "animation" function. It takes a 
  number (the step) and you must return a picture that 
  represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` 
  to test your implementation.

  [JUSTIFY]
  
  I began my implementation of this exercise by creating functions to: a) move the image of the ant in relation to the current
  position and b) show a visual representation of the black squares.

  I started step b) by implementing makeBlackSquare, which uses the rect function to create a 10x10 square, then offsets it by 
  (x*10) and (y*10) - x and y are the co-ordinates of the black square, as stored in the set blackSquares. Each of these are multiplied
  by 10 to ensure the visual representation is correct, as the squares is 10x10 in the image.

  Next, I made the function buildBlackSquaresList, which creates a list of images of every currently active black square. This is
  achieved using list comprehension. I converted the set blackSquares into a list using the data.set function toList. Then, 
  makeBlackSquare is applied to every x and y co-ordinate of elements in this blackSquares list. 
  
  After the list of images is created, the function combineBlackSquares is used to combine all of these images into one. This is 
  achieved using recursion. The recursive call combines the first image in the list with another call to combineBlackSquares 
  using <@>, except with the list argument not containing the first element. The base case is met when every element has been 
  combined into one image (meaning an attempt to call combineBlackSquares on an empty list has been made) - in this case blank 
  is returned and the recursive function unravels.

  I began implementing step a) by breaking down the problem - the ant needs to be able to move between squares, but also rotate
  to face the correct direction. The function rotateAnt applies the Hatch function rotate to the image ant. The angle of rotation
  parameter is given by calling the function angleFromDirection. This function uses a case...of... statement to return the degrees
  the image needs to rotate to face a specific direction. I used a case...of... rather than pattern matching or guards
  for simplicity and readability. 

  I then made the function antMovement to bring all of this together. I combined all of this functionality into one so that the actual
  animation function would look cleaner. antMovement takes an AntState and uses rotateAnt to rotate an image of the ant into the
  specified direction. Then, offset is applied to this image by x*10 and y*10. This will move the ant to its correct co-ordinate.

  Now, to bring this all together, I made the getState function. This is used to compute the next ant state using the step function.
  It takes an int as an argument, which will represent a particular frame of the animation. getState is a recursive
  function, where the base case is met when the animation is on frame 0, in which case the initial state is returned. Otherwise, the
  step function is applied to getState at the previous frame.

  For example:
  Suppose t = 0
    Returned: initialState

  Suppose t = 1
    Returned: step (getState (0))
      = step (initialState) 

  Suppose t = 2
    Returned: step (getState (1)) 
      = step (step (getState (0)))
        = step (step (initialState))
      
  => Therefore t directly correlates to the number of times step has been applied to initial state. So, every frame of the animation
  will be a new state for the ant. 

  Finally, the animation function itself combines the ant image with the image of all the black squares using <@>. This 
  operator ensures that the ant is infront of the black squares. The black squares are displayed by applying combineBlackSquares
  to the function buildBlackSquaresList, with the argument of state. State is a constant referenced in the where clause (for better
  readability), and it is a call to getState with the argument being t (the current frame of the animation). The ant is displayed 
  above the black squares by applying antMovement to state.

  Because this exercise uses lots of smaller functions, I used a where clause to organise them. This makes it visually clearer
  which functions are being used where.

-}



animation :: Int -> Image 
animation t = combineBlackSquares (buildBlackSquaresList state) <@> antMovement state 
  where 
    state = getState t 

    getState :: Int -> AntState 
    getState t 
      | t == 0 = initialState
      | otherwise = step $ getState (t - 1)

    antMovement :: AntState -> Image
    antMovement (AS direction (x, y) blackSquares) = offset (x*10) (y*10) (rotateAnt direction)
      where
        rotateAnt :: Direction -> Image
        rotateAnt direction = rotate (angleFromDirection direction) ant
          where
            angleFromDirection :: Direction -> Int 
            angleFromDirection x = case x of
              North -> 0
              East -> 90
              South -> 180
              West -> 270

    combineBlackSquares :: [Image] -> Image
    combineBlackSquares [] = blank
    combineBlackSquares (x:xs) = x <@> combineBlackSquares xs

    buildBlackSquaresList :: AntState -> [Image]
    buildBlackSquaresList (AS direction current blackSquares) = [ makeBlackSquare x y | (x, y) <- Set.toList blackSquares ]
      where
        makeBlackSquare :: Int -> Int -> Image
        makeBlackSquare x y = offset (x*10) (y*10) (rect 10 10)
