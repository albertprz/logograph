package com.albertoperez1994.scalaql.utils

import com.albertoperez1994.scalaql.core.Identity
import com.albertoperez1994.scalaql.config.ScalaQLConfig
import StringUtils.*


  trait PrettyPrintTree:

    override def toString() = show(this, 0)

    private def show(value: Any, depth: Int): String =

      val result = value match
        case opt: Option[?] => opt.fold("") (show(_, depth + 1))
        case dict: Map[?, ?] => showMap(dict, depth + 1)
        case seq: Seq[?]   => showSeq(seq, depth + 1)
        case prod: Product  => showProduct(prod, depth + 1)
        case other @ _ => other.toString

      result.replace(", ,", ",")


    private def showProduct (prod: Product, depth: Int) =

      val className = prod.getClass.getSimpleName
      val iter = prod match
        case ident: Identity => ident.sql
        case _ =>  concatStr(prod.productIterator, depth)

      val str = if !className.contains("Tuple") then s"$className ($iter)"
              else iter
      val indentation = " " * depth

      if str.size < 25 then str
      else s"\n$indentation$str"


    private def showMap (dict: Map[?, ?], depth: Int) =
      val dictString = dict.asInstanceOf[Map[Any, Any]]
                           .map { (k, v) => (show(k, depth), show(v, depth)) }
                           .map { (k, v) => s"$k -> $v" }
                           .mkString(", ")
      s"{$dictString}"


    private def showSeq (seq: Seq[?], depth: Int) =
      val seqString = concatStr(seq.iterator, depth)
      seqString.wrapBrackets()


    private def concatStr(iter: Iterator[?], depth: Int) =
      iter.map (show(_, depth))
          .mkString(", ")
