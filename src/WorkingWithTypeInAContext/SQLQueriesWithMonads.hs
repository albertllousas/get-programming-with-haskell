{-# LANGUAGE RebindableSyntax #-}
module WorkingWithTypeInAContext.SQLQueriesWithMonads where

import WorkingWithTypeInAContext.MonadTypeClass
import Prelude hiding ((>>=), (>>), return, fail, fmap)

data Name = Name { firstName :: String, lastName :: String } deriving (Eq)

instance Show Name where
    show (Name f l) = mconcat [f, " ", l]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Show)

data Student = Student { studentId :: Int, gradeLevel :: GradeLevel, studentName :: Name }

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
           ,(Student 2 Junior (Name "Leslie" "Silko"))
           ,(Student 3 Freshman (Name "Judith" "Butler"))
           ,(Student 4 Senior (Name "Guy" "Debord"))
           ,(Student 5 Sophomore (Name "Jean" "Baudrillard"))
           ,(Student 6 Junior (Name "Julia" "Kristeva"))]

_select :: MyMonad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

