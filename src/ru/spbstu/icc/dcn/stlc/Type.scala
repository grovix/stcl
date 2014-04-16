package ru.spbstu.icc.dcn.stlc

import scala.util.parsing.combinator._

abstract class Type

// Примитивный тип Int.
case object IntType extends Type {
  override def toString = "Int" 
}

// Примитивный тип Bool.
case object BoolType extends Type {
  override def toString = "Bool"
}

// Конструктор типов функций.
case class FunType(domain: Type, codomain: Type) extends Type {
  override def toString = domain match {
    // Конструктор типов "->" правоассоциативен.
    // Если первый (левый) аргумент этого конструктора сам
    // имеет тип функции, его нужно заключить в скобки.
    case FunType(_, _) => s"($domain) -> $codomain"

    // Лишние скобки не ставим - они подразумеваются. 
    case _ => s"$domain -> $codomain"
  }
}

// Конструктор типов упорядоченных пар.
case class PairType(fst: Type, snd: Type) extends Type {
  override def toString = s"{$fst, $snd}" 
}
