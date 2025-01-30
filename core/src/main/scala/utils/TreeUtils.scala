package com.albertprz.logograph.utils


enum Tree[T]:

  case Leaf[T] (value: T)                         extends Tree[T]
  case Node[T] (value: T, children: Seq[Tree[T]]) extends Tree[T]


  def map[U] (mapFn: T => U) : Tree[U] =
    this match
      case Tree.Leaf(value)           => Leaf(mapFn(value))
      case Tree.Node(value, children) => Node(mapFn(value), children map (_ map mapFn))

  def foldLeft[U] (init: U) (reduceFn: (U, T) => U): U =
    this match
      case Tree.Leaf(value)           => reduceFn(init, value)
      case Tree.Node(value, children) => (children :+ Leaf(value)).foldLeft(init) {
        case (acc, curr) => curr.foldLeft(acc)(reduceFn)
      }

  def foldLeftWithIndex[U] (init: U) (reduceFn: ((U, Int), T) => U): U =
    var i = 0

    foldLeft(init) {
      (acc, curr) => { i += 1; reduceFn((acc, i), curr) }
    }


object TreeUtils:

  import Tree.*

  def buildTree[T] (treeObject: T) (treeFn: T => Seq[T]): Tree[T] =
    if treeFn(treeObject).isEmpty then
      Leaf(treeObject)
    else
      Node(treeObject, treeFn(treeObject) map (buildTree (_) (treeFn)))
