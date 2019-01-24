package runtime

import schröder.Schröder
import bib.Bib._
import scala.collection.mutable.ListBuffer

object main
{
    def main(args: Array[String]): Unit =
    {
        val T = new Schröder
        var res = T.unrankTreeStrong(5,ComptageStrong(2) - 1)
        println("res : "+res._1)
    }
}
