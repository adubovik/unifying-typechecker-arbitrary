module Test(main) where

import Prelude hiding(exp)

import ListLogic
import FromList

import Type
import Exp
import Language
import BiIsoM

testTypeCheck :: IO ()
testTypeCheck = do
  let test :: Exp -> [Type] -> IO ()
      test expr res =
        print $ runForward exp expr == ((fromList res) :: LL Type)
  test (Lit 1) [TInt]
  test (Add (Lit 1) (Lit 2)) [TInt]
  test (Eq (Lit 1) (Lit 2)) [TBool]
  test (Eq (Lit 1) (Eq (Lit 1) (Lit 3))) []
  test (Eq (Eq (Lit 2) (Lit 4)) (Eq (Lit 1) (Lit 3))) [TBool]

arbitraryCheck :: IO ()
arbitraryCheck = do
  let run f = do
        let es = take 20 $ unLL $ f
        putStrLn $ show (length es) ++ ":\n" ++
                   unlines (map show es)

  run $ runBackward exp TInt
  run $ runBackward exp TBool

main :: IO ()
main = do
  testTypeCheck
  arbitraryCheck
