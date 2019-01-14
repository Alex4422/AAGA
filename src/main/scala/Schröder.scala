package schröder

import scala.collection.mutable.ListBuffer

class Schröder
{
    case class Node(fils: ListBuffer[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    def treeBuilder(n: Int): Schröder = //n -> Taille de l'arbre = nb Feuille
    {
        if (n <= 1) Leaf()

        val rand = scala.util.Random
        var T = Node(ListBuffer(Leaf(), Leaf()), 1)
        var tmp = T
        var last = T
        var l = 2
        var k = 0
        var newNode = Leaf().asInstanceOf[Schröder]
        var index = 0

        var listLeaf = ListBuffer(T, T)

        for(i <- 3 to n)
        {
            k = rand.nextInt(i - 1)
            if(k == i)
            {
                last.fils.append(Leaf())
                listLeaf.append(last)
            }
            else
            {
                println("k : "+k+" list : "+listLeaf.size)

                tmp = listLeaf.remove(k)

                for(j <- tmp.fils) j match
                {
                    case Leaf() => newNode = j
                    case _ =>
                }

                index = tmp.fils.indexOf(newNode)
                tmp.fils -= newNode
                tmp.fils.insert(index, Node(ListBuffer(Leaf(), Leaf()), l))
                last = tmp.fils.apply(index).asInstanceOf[Schröder.this.Node]

                listLeaf.insert(k, last)
                listLeaf.append(last)

                l += 1
            }
        }

        T
    }
}
