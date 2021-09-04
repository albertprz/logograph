package com.albertoperez1994.scalaql.utils


sealed trait Tree [T] {

  def map[U] (mapFn: T => U) : Tree[U] = {
    this match {
      case Leaf(value) => Leaf(mapFn(value))
      case Node(value, children) => Node(mapFn(value), children map (_ map mapFn))
    }
  }

  def foldLeft[U] (init: U) (reduceFn: (U, T) => U): U = {
    this match {
      case Leaf(value) => reduceFn(init, value)
      case Node(value, children) => (children :+ Leaf(value)).foldLeft(init) {
        case (acc, curr) => curr.foldLeft(acc)(reduceFn)
      }
    }
  }

  def foldLeftWithIndex[U] (init: U) (reduceFn: ((U, Int), T) => U): U = {
    var i = 0

    foldLeft(init) {
      case (acc, curr) => { i += 1; reduceFn((acc, i), curr) }
    }
  }
}

case class Leaf [T] (value: T)
    extends Tree [T]
case class Node [T] (value: T, children: Seq[Tree[T]])
    extends Tree [T]

object TreeUtils {

  def buildTree[T] (treeObject: T) (treeFn: T => Seq[T]): Tree[T] = {
    if (treeFn(treeObject).isEmpty) {
      Leaf(treeObject)
    }
    else {
      Node(treeObject, treeFn(treeObject) map (buildTree (_) (treeFn)))
    }
  }
}
