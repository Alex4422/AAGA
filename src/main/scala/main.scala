package runtime

import schröder.Schröder

object main
{
    def main(args: Array[String]): Unit =
    {
        val T = new Schröder
        println(T.treeBuilder(5))
    }
}
