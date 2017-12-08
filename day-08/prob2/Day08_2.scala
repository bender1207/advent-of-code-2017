package day08_2

import scala.collection.mutable
import scala.io.Source

abstract class Operation {
  val reg: String
  val value: Int

  val registerValueMap: collection.mutable.Map[String, Int]

  def apply()
}

case class Addition(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends Operation {
  override def apply() = {
    registerValueMap.put(reg, registerValueMap.apply(reg) + value)
  }
}

case class Subtraction(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends Operation {
  override def apply() = {
    registerValueMap.put(reg, registerValueMap.apply(reg) - value)
  }
}

abstract class OperationPredicate {
  val reg: String
  val value: Int

  val registerValueMap: collection.mutable.Map[String, Int]

  def apply() : Boolean
}

case class PredicateLT(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) < value
  }
}

case class PredicateGT(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) > value
  }
}

case class PredicateLE(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) <= value
  }
}

case class PredicateGE(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) >= value
  }
}

case class PredicateEQ(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) == value
  }
}

case class PredicateNE(reg: String, value: Int, registerValueMap: collection.mutable.Map[String, Int]) extends OperationPredicate {
  override def apply() = {
    registerValueMap.apply(reg) != value
  }
}


object Day08_2 extends App
{
  def parseRegister(row: String) : String = row.split(" ").head

  def createOperation(tokens: Seq[String], registerValueMap: mutable.Map[String, Int]) : Operation =
  {
    tokens(1) match {
      case "inc" => Addition(tokens.head, tokens(2).toInt, registerValueMap)
      case "dec" => Subtraction(tokens.head, tokens(2).toInt, registerValueMap)
    }
  }

  def createOperationPredicate(tokens: Seq[String], registerValueMap: mutable.Map[String, Int]) : OperationPredicate =
  {
    tokens(1) match {
      case "<" => PredicateLT(tokens.head, tokens(2).toInt, registerValueMap)
      case ">" => PredicateGT(tokens.head, tokens(2).toInt, registerValueMap)
      case "<=" => PredicateLE(tokens.head, tokens(2).toInt, registerValueMap)
      case ">=" => PredicateGE(tokens.head, tokens(2).toInt, registerValueMap)
      case "==" => PredicateEQ(tokens.head, tokens(2).toInt, registerValueMap)
      case "!=" => PredicateNE(tokens.head, tokens(2).toInt, registerValueMap)
    }
  }

  def getMax(registerValueMap: collection.mutable.Map[String, Int]) : Int = registerValueMap.maxBy(_._2)._2

  def executeInstruction(row: String, registerValueMap: mutable.Map[String, Int], maxValue: Int) : Int =
  {
    val tokens = row.split(" ")
    val operation = createOperation(tokens.toList.slice(0, 3), registerValueMap)
    val predicate = createOperationPredicate(tokens.toList.slice(4, tokens.length), registerValueMap)

    if (predicate.apply) {
      operation.apply
    }

    math.max(getMax(registerValueMap), maxValue)
  }

  def getMaxRegisterValueDuringInstructions(rows: Seq[String]) : Int =
  {
    val registerValueMap: mutable.Map[String, Int] = collection.mutable.Map(rows.map(parseRegister).map((_, 0)): _*)
    rows.foldLeft(0){ (z, r) => executeInstruction(r, registerValueMap, z) }
  }


  // -- Tests -------------------
  {
    val rows = Seq(
      "b inc 5 if a > 1",
      "a inc 1 if b < 5",
      "c dec -10 if a >= 1",
      "c inc -20 if c == 10")

    val max = getMaxRegisterValueDuringInstructions(rows)
    assert(max == 10)
  }
  // ----------------------------


  // Read input
  //
  val rows: Seq[String] = Source.fromResource("day08.txt").getLines.toSeq

  val max = getMaxRegisterValueDuringInstructions(rows)

  println(max)
}
