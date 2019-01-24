package runtime

import schröder.Schröder
import bib.Bib._
import scala.collection.mutable.ListBuffer

object main
{
    def main(args: Array[String]): Unit =
    {
        val T = new Schröder
        var res = T.unrankTreeStrong(6,ComptageStrong(2) - 1)
        println("Root : "+res._1)
        println("Last : "+res._2)
    }
}
