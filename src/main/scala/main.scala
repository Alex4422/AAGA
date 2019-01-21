package runtime

import schröder.Schröder
import bib.Bib._
import scala.collection.mutable.ListBuffer

object main
{
    def main(args: Array[String]): Unit =
    {
        val T = new Schröder
        println(T.unrankTree(2, ComptageWeak(2) - 1))
    }
}
