package ru.ifmo.onell

import java.io.PrintStream

import ru.ifmo.onell.main._

object Main {
  trait Module {
    def name: String
    def shortDescription: String
    def longDescription: Seq[String]
    def moduleMain(args: Array[String]): Unit
  }

  private val modules = Seq(Help, RunningTimes, FixedBudget, FixedTarget, FixedTargetLambda, IRaceClient, LambdaColorMap, LambdaTraces)

  private def printCommandList(stream: PrintStream, prefix: String): Unit = {
    val moduleNameWidth = modules.view.map(_.name.length).max
    for (m <- modules) {
      stream.print(prefix)
      stream.print(" " * (moduleNameWidth - m.name.length))
      stream.print(m.name)
      stream.print(": ")
      stream.println(m.shortDescription)
    }
  }

  object Help extends Main.Module {
    override def name: String = "help"
    override def shortDescription: String = "Prints this help (try help <command> to get help for that command)"
    override def longDescription: Seq[String] = Seq(
      "Without arguments, prints the list of possible commands.",
      "When invoked as 'help <command>, prints the help for that command."
    )

    override def moduleMain(args: Array[String]): Unit = {
      if (args.length == 0) printCommandList(System.out, "")
      else modules.find(_.name == args(0)) match {
        case Some(m) => m.longDescription.foreach(println)
        case None =>
          println("No such command: '" + args(0) + "'. Possible commands are:")
          printCommandList(System.out, "  ")
      }
    }
  }

  private def usage(): Nothing = {
    System.err.println("Usage: Main <command> [args...], where <command> is one of:")
    printCommandList(System.err, "  ")
    sys.exit()
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage()
    } else modules.find(_.name == args(0)) match {
      case Some(m) => m.moduleMain(args.tail)
      case None => usage()
    }
  }
}
