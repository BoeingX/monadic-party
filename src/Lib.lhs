\documentclass[english]{beamer}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{babel}
\usepackage{minted}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{slide}[1]
  {\begin{frame}[fragile,environment=slide]{#1}}
  {\end{frame}}
\long\def\ignore#1{}
\newcommand{\credit}[1]{\par\hfill \footnotesize Credit:~\itshape#1}


\mode<presentation>
{
  \usetheme{Warsaw}
  \usecolortheme{beaver}
  \usefonttheme{serif}
}

\title{Monadic Party}
\subtitle{From Functor to Monad}
\author{Baoyang \textsc{Song}}
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
module Lib where

import Control.Applicative
import Control.Monad
\end{code}
}

\begin{frame}
	\titlepage
\end{frame}

\section{Haskell in a nutshell}

\begin{slide}{Function and currying}
\begin{spec}
add :: Integer -> Integer -> Integer
add x y =  x + y

add' :: Integer -> Integer -> Integer
add' =  (+)

addOne :: Integer -> Integer
addOne = (+) 1

addTwoOnes :: Integer
addTwoOnes = addOne 1
\end{spec}
\end{slide}

\begin{slide}{Abstract data type}
\begin{spec}
-- trivial ones
data Trivial = Trivial
data Identity a = Identity a
data Pair a = Pair a a

-- simple ones
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

-- recursion!
data List a = Nil | Cons a (List a)
data Tree a = Leaf | Node a (Tree a) (Tree a)
\end{spec}
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

\section{From Functor to Monad}

\subsection{Kind}

\begin{slide}{Kind}
TODO
\end{slide}

\subsection{Functor}

\begin{slide}{Definition}
\begin{spec}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
\end{spec}
\end{slide}

\begin{slide}{Laws}
\begin{spec}
-- identity
fmap id = id

-- composition
fmap (g . h) = (fmap g) . (fmap h)
\end{spec}
\begin{itemize}
\item A given type has at most one valid instance of Functor (\href{http://article.gmane.org/gmane.comp.lang.haskell.libraries/15384}{proof})
\item Any Functor instance satisfying the first law (fmap id = id) will automatically satisfy the second law (\href{https://github.com/quchen/articles/blob/master/second\_functor\_law.md}{proof})
\end{itemize}
\end{slide}

\begin{slide}{Instances - Maybe}
\begin{spec}
instance Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing

Prelude> fmap (+1) (Just 1)
Just 2
Prelude> fmap (+1) Nothing
Nothing
\end{spec}
\end{slide}

\begin{slide}{Instances - Tree}
\begin{spec}
instance Tree where
    fmap _ Leaf = Leaf
    fmap f (Node a lTree rTree) = Node (f a) (fmap f lTree) (fmap f rTree)

Prelude> fmap (*2) Leaf
Leaf
Prelude> fmap (*2) (Node 2 (Node 1) Leaf)
Node 4 (Node 2) Leaf
\end{spec}
\end{slide}

\begin{slide}{Instances - Either e}
\begin{spec}
instance Either e where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left e)     = Left e
    fmap f (Right a)    = Right (f a)

Prelude> fmap (+1) (Left "a")
Left "a"
Prelude> fmap (+1) (Right 1)
Right 2
\end{spec}
\end{slide}

\subsection{Applicative}
\begin{slide}{Definition}
\begin{spec}
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
\end{spec}
\end{slide}

\begin{slide}{Laws}
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

\begin{slide}{Maybe is an applicative}
\begin{spec}
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _       = Nothing
    Just f  <*> x       = fmap f x

Prelude> pure 1 :: Maybe Int
Just 5
Prelude> Just (+1) <*> Just 2
Just 3
Prelude> Just (++ " world") <*> Just "Hello"
Just "Hello world"
\end{slide}

\begin{slide}{Either a is an applicative}
\begin{spec}
instance Applicative (Either a) where
    pure :: b -> Either a b
    pure = Right

    (<*>) :: Either a (b -> c) -> Either a b -> Either a c
    Left a  <*> _ = Left a
    Right f <*> x = fmap f x

\end{spec}
\end{slide}

\begin{slide}{List is an applicative}
\begin{spec}
instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = concatMap (\f -> map f xs) fs
    -- or with list comprehension
    -- fs <*> xs = [f x | f <- fs, x <- xs]
\end{spec}
\end{slide}

\subsection{Monad}
\begin{slide}{Definition}
\begin{spec}
class (Applicative m) => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b
\end{spec}
\end{slide}

\begin{slide}{Laws}
TODO
\end{slide}

\begin{slide}{Instances - Maybe}
TODO
\end{slide}

\subsection{Comparison}

\begin{slide}{Comparison}
\begin{spec}
-- Functor
-- (<$>) = fmap
(<$>) ::   (a -> b) -> f a -> f b

-- Applicative
(<*>) :: f (a -> b) -> f a -> f b

-- Monad
-- (=<<) = flip . (>>=)
(=<<) :: (a -> m b) -> m a -> m b
\end{spec}
\end{slide}

\end{document}
