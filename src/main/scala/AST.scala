package org.bigraph.bigmc

class Control(name : String, active : Boolean) {
    override def toString = name
}

class Name(index : Int) {
    override def toString = "@" + index
}

sealed trait Term {
    def toString : String
}

case class Prefix(ctrl : Control, ports : Seq[Name], suffix : Term) extends Term {
    override def toString = ctrl + "<" + ports.mkString(",") + ">." + suffix
}

case class Par(terms : Seq[Term]) extends Term {
    override def toString = "(" + terms.mkString(" | ") + ")"
}

case class WPar(terms : Seq[Term]) extends Term {
    override def toString = "(" + terms.mkString(" || ") + ")"
}

case class Hole(index : Int) extends Term {
    override def toString = "$" + index
}

case class Alias(inner : Seq[Name], outer : Seq[Name]) extends Term {
    override def toString = "[" + inner.mkString(",") + " |-> " + outer.mkString + "]"
}

case class Local(name : Name, body : Term) extends Term {
    override def toString = "\\" + name + " " + body 
}

case class TNil extends Term {
    override def toString = "nil"
}
