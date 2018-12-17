\documentclass[english,10pt]{beamer}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{babel}
\usepackage{minted}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}[breaklines]{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}[breaklines]{haskell}}{\end{minted}}
\newenvironment{slide}[1]
  {\begin{frame}[fragile,environment=slide]{#1}}
  {\end{frame}}
\long\def\ignore#1{}
\newcommand{\credit}[1]{\par\hfill \footnotesize Credit:~\itshape#1}

\newtheorem{remark}{remark}
\newenvironment<>{remark}[1]{%
  \setbeamercolor{block title}{fg=white,bg=red!75!black}%
  \begin{block}{Remark}{#1}}{\end{block}}

\newenvironment<>{problem}[1]{%
  \setbeamercolor{block title}{fg=white,bg=red!75!black}%
  \begin{block}{Problem}{#1}}{\end{block}}


\mode<presentation>
{
  \usetheme{Warsaw}
  \usecolortheme{beaver}
  \usefonttheme{serif}
}

\title{Monadic Party}
\subtitle{From Functor to Monad}
\date{\today}

\AtBeginSection[]
{
  \begin{frame}<beamer>{Outline}
    \tableofcontents[currentsection]
  \end{frame}
}
\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE InstanceSigs #-}
module Lib where

import Prelude hiding (Maybe(..),Either(..))
import Control.Applicative
import Control.Monad
\end{code}
}

\begin{frame}
	\titlepage
\end{frame}

\section{Basic Syntax}

\begin{slide}{Function and currying}
\begin{code}
add :: Integer -> Integer -> Integer
add x y =  x + y

add' :: Integer -> Integer -> Integer
add' =  (+)

addOne :: Integer -> Integer
addOne = (+) 1

addTwoOnes :: Integer
addTwoOnes = addOne 1
\end{code}
\end{slide}

\begin{slide}{Abstract data type}
\begin{code}
data Trivial = Trivial
data Identity a = Identity a
data Pair a = Pair a a

data Maybe a = Nothing | Just a
    deriving (Eq, Show)
data Either a b = Left a | Right b
    deriving (Eq, Show)

-- | [] and (:) are built-in syntax
-- so we cannot "redefine" list using
-- data [] a = [] | a : [a]
data List a = Nil | Cons a (List a)
data Tree a = Leaf | Node a (Tree a) (Tree a)
\end{code}
\end{slide}

\section{Typeclassopedia}

\begin{slide}{Typeclass}
\begin{spec}
class Eq a where
    (==), (/=)           :: a -> a -> Bool
    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}

instance (Eq a) => Eq (Maybe a) where
    Just x  == Just y   = x == y
    Nothing == Nothing  = True
    _       == _        = False
\end{spec}
\end{slide}

\begin{frame}{Typeclassopedia}
\begin{figure}
\centering
\includegraphics[width=\linewidth]{./docs/imgs/Typeclassopedia-diagram.png}
\credit{\href{https://wiki.haskell.org/Typeclassopedia}{https://wiki.haskell.org/Typeclassopedia}}
\caption{Typeclass hierarchy in Haskell}
\end{figure}
\end{frame}

\section{Functor, Applicative and Monad}

\begin{slide}{Kind}
\begin{definition}
A kind is the type of a type constructor or, less commonly, the type of a higher-order type operator. 
\end{definition}
\begin{example}
\begin{spec}
Int :: *

Maybe :: * -> *
Maybe Bool :: *

Either :: * -> * -> *
Either String -> * -> *
Either String Int -> *

[] :: * -> *
[Int] :: *

\end{spec}
\end{example}
\end{slide}

\subsection{Functor}

\begin{slide}{Functor}
\begin{definition}
\begin{spec}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
\end{spec}
\end{definition}

\begin{remark}
A functor \mintinline{haskell}{f} must be of kind \mintinline{haskell}{* -> *}.
\end{remark}

Common instances: \mintinline{haskell}{[], Maybe, Either e, IO, (->) r}
\end{slide}

\begin{slide}{Functor - Laws}
\begin{spec}
-- identity
fmap id = id

-- composition
fmap (g . h) = (fmap g) . (fmap h)
\end{spec}
\begin{block}{Property}
A given type has at most one valid instance of Functor (\href{http://article.gmane.org/gmane.comp.lang.haskell.libraries/15384}{proof})
\end{block}

\begin{block}{Property}
Any Functor instance satisfying the first law (fmap id = id) will automatically satisfy the second law (\href{https://github.com/quchen/articles/blob/master/second\_functor\_law.md}{proof})
\end{block}
\end{slide}

\begin{slide}{Functor instances - Maybe}
\begin{definition}
\begin{code}
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing
\end{code}
\end{definition}

\begin{example}
\begin{spec}
Prelude> fmap (+1) (Just 1)
Just 2
Prelude> fmap (+1) Nothing
Nothing
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Functor instances - Either e}
\begin{definition}
\begin{code}
instance Functor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left e)     = Left e
    fmap f (Right a)    = Right (f a)
\end{code}
\end{definition}

\begin{example}
\begin{spec}
Prelude> fmap (+1) (Left "a")
Left "a"
Prelude> fmap (+1) (Right 1)
Right 2
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Functor instances - []}
\begin{definition}
\begin{spec}
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map
\end{spec}
\end{definition}

\begin{example}
\begin{spec}
Prelude> fmap (+1) [1,2,3]
[2,3,4]
\end{spec}
\end{example}
\end{slide}

\subsection{Applicative}

\begin{slide}{Applicative}
\begin{definition}
\begin{spec}
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
\end{spec}
\end{definition}
\begin{remark}
An Applicative \mintinline{haskell}{f} must be of kind \mintinline{haskell}{* -> *}.
\end{remark}
Common instances: \mintinline{haskell}{[], Maybe, Either e, IO, (->) r}
\end{slide}

\begin{slide}{Applicative laws}
\begin{spec}
-- identity
pure id <*> v = v
-- homomorphism
pure f <*> pure x = pure (f x)
-- interchange
u <*> pure y = pure ($ y) <*> u
-- composition
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
\end{spec}
\end{slide}

\begin{slide}{Applicative instances - Maybe}
\begin{definition}
\begin{code}
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _       = Nothing
    Just f  <*> x       = fmap f x
\end{code}
\end{definition}

\begin{example}
\begin{spec}
Prelude> pure 1 :: Maybe Int
Just 5
Prelude> Just (+1) <*> Just 2
Just 3
Prelude> Just (++ " world") <*> Just "Hello"
Just "Hello world"
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Applicative instances - Either e}
\begin{definition}
\begin{code}
instance Applicative (Either e) where
    pure :: a -> Either e a
    pure = Right

    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    Left e  <*> _ = Left e
    Right f <*> a = fmap f a
\end{code}
\end{definition}
\begin{example}
\begin{spec}
Prelude> Left "hello" <*> Right "world"
Left "hello"
Prelude> Right ("hello " ++ ) <*> Right "world"
Right "hello world"
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Applicative instances - []}
\begin{definition}
\begin{spec}
instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]
\end{spec}
\end{definition}
\begin{example}
\begin{spec}
Prelude> [(+1), (+2), (+3)] <*> [1, 2]
[2,3,3,4,4,5]
Prelude> (+) <$> [1,2,3] <*> [1, 2]
[2,3,3,4,4,5]
Prelude> (,) <$> [1,2,3] <*> ['a', 'b']
[(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
\end{spec}
\end{example}
\end{slide}

\subsection{Monad}
\begin{slide}{Monad}
\begin{definition}
\begin{spec}
class (Applicative m) => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    a >> b = a >>= \_ -> b
\end{spec}
\end{definition}
\begin{remark}
A Monad \mintinline{haskell}{m} must be of kind \mintinline{haskell}{* -> *}.
\end{remark}
Common instances: \mintinline{haskell}{[], Maybe, Either e, IO, (->) r}
\end{slide}

\begin{slide}{Do notation}
\begin{itemize}
\item Do-notation is a syntax sugar for writing monad syntax \mintinline{haskell}{(>>)} and \mintinline{haskell}{(>>=)}
\item It is desugared recursively during compilation
\end{itemize}

\begin{overprint}
\onslide<1>
Desugaring the \mintinline{haskell}{(>>)} operator
\begin{columns}[T] % align columns
\begin{column}{.4\textwidth}
\begin{block}{Sugar}
\begin{spec}
do
    action1
    action2
    action3
\end{spec}
\end{block}
\end{column}%
\hfill%
\begin{column}{.4\textwidth}
\begin{block}{Desugared}
\begin{spec}
action1 >>
do
    action2
    action3
\end{spec}
\end{block}
\end{column}%
\end{columns}

\onslide<2>
Desugaring the \mintinline{haskell}{(>>=)} operator
\begin{columns}[T] % align columns
\begin{column}{.4\textwidth}
\begin{block}{Sugar}
\begin{spec}
do
    x1 <- action1
    x2 <- action2
    mk_action3 x1 x2
\end{spec}
\end{block}
\end{column}%
\hfill%
\begin{column}{.4\textwidth}
\begin{block}{Desugared}
\begin{spec}
action1 >>= (\ x1 ->
  action2 >>= (\ x2 ->
    mk_action3 x1 x2 ))
\end{spec}
\end{block}
\end{column}%
\end{columns}

\end{overprint}
\end{slide}

\begin{slide}{Monad Laws}
\begin{spec}
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
\end{spec}
\end{slide}

\begin{slide}{Monad instances - Maybe}
\begin{definition}
\begin{code}
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Just x  >>= f = f x
    Nothing >>= _ = Nothing
\end{code}
\end{definition}

\begin{example}
\begin{spec}
Prelude> Just 1 >>= \x -> Just (x + 1)
Just 2
Prelude> Just 1 >>= \x -> Nothing
Nothing
Prelude> Nothing >>= \x -> Just (x + 1)
Nothing
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Monad instances - Either e}
\begin{definition}
\begin{code}
instance Monad (Either e) where
    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    Left e  >>= f = Left e
    Right a >>= f = f a
\end{code}
\end{definition}
\begin{example}
\begin{spec}
Prelude> Left "hello" >>= \x -> Right (length x)
Left "hello"
Prelude> Right "world" >>= \x -> Right (length x)
Right 5
Prelude> Right "world" >>= \x -> Left "hello"
Left "hello"
\end{spec}
\end{example}
\end{slide}

\begin{slide}{Monad instances - []}
\begin{definition}
\begin{spec}
instance Monad [] where
    (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concatMap f xs
\end{spec}
\end{definition}

\begin{example}
\begin{spec}
Prelude> [1, 2, 3] >>= \x -> [x]
[2,3,4]
Prelude> [1, 2, 3] >> \x -> replicate x x
[1,2,2,3,3,3]
\end{spec}
\end{example}
\end{slide}


\section{Discussion}

\subsection{Why bother?}
\begin{slide}{Functor}
\begin{problem}
Implement the following function, which process the response of a HTTP request
\begin{code}
handleResponse :: (String -> String) -> Maybe String -> Maybe String
\end{code}
\end{problem}

\begin{overprint}

\onslide<1>
Without functor
\begin{spec}
handleResponse f response = case response of
    Nothing  -> Nothing
    Just res -> Just (f res)
\end{spec}

\onslide<2>
With functor
\begin{code}
handleResponse = fmap
\end{code}
\end{overprint}

\end{slide}

\begin{slide}{Applicative}

\begin{problem}

Given the following types
\begin{code}
type Name  = String
type Age   = Int
data Error = InvalidName | InvalidAge
data User  = User Name Age
\end{code}
and the following validation functions
\begin{code}
validateName :: Name -> Either Error Name
validateAge :: Age -> Either Error Age
\end{code}
\ignore{
\begin{code}
validateName = undefined
validateAge = undefined
\end{code}
}

Implement a constructor
\begin{code}
mkUser :: Name -> Age -> Either Error User
\end{code}
\end{problem}
\end{slide}

\begin{slide}{Applicative (cont'd)}
Without Applicative
{\small
\begin{spec}
mkUser name age = case validateName name of
    Left InvalidName -> Left InvalidName
    _                -> case validateAge age of
                          Left InvalidAge -> Left InvalidAge
                          _               -> Right (User name age)
\end{spec}
}
\end{slide}

\begin{slide}{Applicative (cont'd)}
With Applicative
\begin{code}
mkUser name age =
    User <$> validateName name <*> validateAge age
\end{code}
\end{slide}

\begin{slide}{Monad}
TODO
\end{slide}

\subsection{Fundamental difference}

\begin{slide}{A sliding scale of power}
\texttt{fmap}, \texttt{apply} and \texttt{bind}
\begin{spec}
(<$>) :: Functor t     => (a -> b)   -> t a -> t b
(<*>) :: Applicative t => t (a -> b) -> t a -> t b
(=<<) :: Monad t       => (a -> t b) -> t a -> t b
\end{spec}

\begin{overprint}
\onslide<1>
\begin{block}{Property}
\mintinline{haskell}{fmap} cannot change the context \mintinline{haskell}{t}.
\end{block}

\begin{spec}
Prelude> fmap (2*) [2,5,6]
[4,10,12]
\end{spec}

\onslide<2>
\begin{block}{Property}
The changes to the context \mintinline{haskell}{(<*>)} performs are fully determined by the context \mintinline{haskell}{t} of its arguments.
\end{block}

\begin{spec}
Prelude> [(2*),(3*)] <*> [2,5,6]
[4,10,12,6,15,18]
Prelude> [(2+),(3+)] <*> [2,5,6]
[4,7,8,5,8,9]
\end{spec}

\onslide<3>
\begin{block}{Property}
\mintinline{haskell}{(>>=)} is able to create context from values.
\end{block}
\begin{spec}
Prelude> [1,2,5] >>= \x -> replicate x x
[1,2,2,5,5,5,5,5]
Prelude> [0,0,0] >>= \x -> replicate x x
[]
\end{spec}

\end{overprint}

\end{slide}

\end{document}
