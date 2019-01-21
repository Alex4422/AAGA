package runtime

import schröder.Schröder
import scala.collection.mutable.ListBuffer

object main
{
    def main(args: Array[String]): Unit =
    {
        val T = new Schröder
        println(T.treeBuilder(5))
    }
}
