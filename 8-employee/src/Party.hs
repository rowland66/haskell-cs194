module Party where

import Data.Tree
import System.IO
import Employee

main :: IO ()
main = do
  treeFileH <- openFile "company.txt" ReadMode
  treeStr <- hGetContents treeFileH
  let gl = maxFun (read treeStr)
  putStrLn ("Total fun: " ++ (show $ (guestListFunScore gl)))
  mapM_ (\emp -> putStrLn (empName emp)) (guestListEmployees gl)
  hClose treeFileH

guestListEmployees (GL l _) = l

guestListFunScore (GL _ f) = f

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  (GL list1 fun1) <> (GL list2 fun2) = GL (list1 <> list2) (fun1 + fun2)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e : list) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if ((compare gl1 gl2) == GT) then gl1 else gl2

nextLevel :: Employee -> [(GuestList, GuestList)]-> (GuestList, GuestList)
nextLevel e [] = (glCons e mempty, mempty)
nextLevel e gs = (glCons e (mostFunGuestListWithMe gs), mostFunGuestListWithoutMe gs)

mostFunGuestListWithMe :: [(GuestList, GuestList)] -> GuestList
mostFunGuestListWithMe gs = foldr (\glDouble gl -> (snd glDouble) <> gl) mempty gs

mostFunGuestListWithoutMe :: [(GuestList, GuestList)] -> GuestList
mostFunGuestListWithoutMe gs = foldr (\glDouble gl -> (moreFun (fst glDouble) (snd glDouble)) <> gl) mempty gs

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst glDouble) (snd glDouble)
           where glDouble = foldTree nextLevel t

foldTree' :: (a -> [b] -> b) -> Tree a -> b
foldTree' f (Node e es) = f e $ (foldTree' f) <$> es

foldFunc :: Tree Employee -> (GuestList, GuestList)
foldFunc (Node e []) = nextLevel e []
foldFunc (Node e es) = nextLevel e $ foldFunc <$> es