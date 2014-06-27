module Test(main) where

import Prelude hiding(exp)

import ListLogic
import FromList

import Type
import Exp
import IsoLanguage
import BiIsoM

testTypeCheck :: IO ()
testTypeCheck = do
  let test :: [Type] -> Exp -> IO ()
      test res expr =
        print $ runForward exp expr == ((fromList res) :: LL Type)
  test [ TInt] (Lit 1)
  test [ TInt] (Add (Lit 1) (Lit 2))
  test [TBool] (Eq (Lit 1) (Lit 2))
  test [TBool] (Eq (Eq (Lit 2) (Lit 4)) (Eq (Lit 1) (Lit 3)))
  test [     ] (Eq (Lit 1) (Eq (Lit 1) (Lit 3)))

testArbitrary :: IO ()
testArbitrary = do
  let run f = do
        let es = take 20 $ unLL $ f
        putStrLn $ show (length es) ++ ":\n" ++
                   unlines (map show es)

  run $ runBackward exp TInt
  run $ runBackward exp TBool

main :: IO ()
main = do
  testTypeCheck
  testArbitrary
