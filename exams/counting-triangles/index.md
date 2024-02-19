---
title: "Exam Task: Counting Triangles"
subtitle: "From: Exam 5 - 2021"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---


\documentclass[11pt]{article}

\title{Counting Triangles (Scheme+Haskell - 7+7 Points)}

\input{../preamble.tex}
\usepackage{tikz}
\usetikzlibrary{arrows,shapes,calc,patterns,angles,quotes}
\newgeometry{vmargin={15mm,20mm}, hmargin={20mm,30mm}}

\begin{document}
\maketitle

An (undirected) graph $G=(V,E)$ is a set of vertices $V$ endowed with a set of edges $E$.
The goal of this task is to implement a function counting a number of triangles 
a given graph has. A triangle is a triplet of pairwise distinct vertices $u,v,w$ such that each pair of them
is connected by an edge. Consider a graph in Figure~\ref{graph}. It has two triangles.
One consisting of vertices $1,2,3$ and one consisting of vertices $1,2,4$.

\begin{figure}[h!]
  \centering
  \begin{tikzpicture}[node distance=2cm,every node/.style={circle,draw}]
      \node (1) {$1$};
      \node[right of=1] (2) {$2$};
      \node[below of=1] (3) {$3$};
      \node[below of=2] (4) {$4$};

      \draw (1) -- (2);
      \draw (1) -- (4);
      \draw (2) -- (4);
      \draw (3) -- (1);
      \draw (3) -- (2);
  \end{tikzpicture}
  \caption{Graph with 2 triangles}
  \label{graph}
\end{figure}

\section{Task 3 - Scheme}%
\label{sec:task_3_scheme}

In Scheme, implement a function \texttt{(count-triangles vs edges)} that counts the number of triangles
of the graph whose vertices are in the list \texttt{vs} and edges are in the list \texttt{edges}.
Each edge is represented as a two element list of vertices. 
You may assume only a valid input, i.e., a list of lists representing a square matrix.
\begin{minted}{Scheme}
> (count-triangles '(1 2 3 4) '((1 2) (1 3) (1 4) (2 3) (2 4)))
2

> (count-triangles '(1 2 3 4) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
4 

> (count-triangles '(1 2) '((1 2) (2 2)))
0
\end{minted}

Your file has to be called \texttt{task3.rkt} and must provide the function
\texttt{count-triangles} so it should start like this:
\begin{minted}{Scheme}
#lang racket
(provide count-triangles)

; your code goes here
\end{minted}


\section{Task 4 - Haskell}%
\label{sec:task_4_haskell}

In Haskell, implement a function \mintinline{haskell}{countTriangles :: Eq a => [a] -> [Edge a] -> Int} 
that accepts a list of vertices of type \texttt{a} and a list of edges over type \texttt{a}
and returns the number of triangles. The type \texttt{Edge a}
is defined as follows:
\mint{haskell}{type Edge a = (a,a)}
You may assume only a valid input.

\begin{minted}{Haskell}
> countTriangles [1..4] [(1,2),(1,3),(1,4),(2,3),(2,4)]
2  

> countTriangles [1..4] [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
4

> countTriangles [1..2] [(1,2),(2,2)]
0
\end{minted}

Your file has to be called \texttt{Task4.hs} and must export the function
\texttt{countTriangles} so it should start like this:
\begin{minted}{Haskell}
module Task4 (countTriangles) where
type Edge a = (a,a)

-- your code goes here
\end{minted}


\end{document}

