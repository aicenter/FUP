---
title: "Exam Task: Tree Intersection Norm"
subtitle: "In Racket and Haskell"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

Suppose we have two binary trees containing integers in their nodes. Find numbers occurring in both
trees and compute their squared norm. More precisely, given a list of common numbers (a1 a2 ... an)
their squared norm is computed as a12+a22 + ... + an2. You may assume that none of the trees
contains any number more than once. 

A binary tree is represented in Scheme as recursive structure starting in the tree’s root, where each node is either ‘null, or represented as
(item left-child right-child)
where
item is the integer value contained in the node;
and left-child and right-child are nodes that are defined either recursively as (item left-child right-child), or as ‘null.

First, implement the function tree2list defined as
(tree2list tree)
where
tree is a binary tree;
and the function returns an ordered list of the numbers occurring in the tree.

Then, implement the function intersect defined as
(intersect list1 list2)
where
list1 and list2 are ordered lists of unique values;
and the function returns an ordered list containing values present in both lists.

Finally, implement the function intersection-norm, which is given as follows:
(intersection-norm tree1 tree2)
where
tree1 and tree2 are binary trees, i.e., are represented as recursive structures of binary trees as defined above;
and the function returns the squared norm of their common numbers, or 0 if there are no common numbers.

Examples

> (define treeA ‘( 1  (2 null null) (3 null null) ) )
> (define treeB ‘( 3  (4 null null) (2 null (5 null null)) ) )

> (tree2list treeA)
‘(1 2 3)

> (tree2list treeB)
‘(2 3 4 5)

> (intersect (tree2list treeA) (tree2list treeB) )
‘(2 3)

> (intersection-norm treeA treeB)
13



