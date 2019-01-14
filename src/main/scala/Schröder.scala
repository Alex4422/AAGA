package schröder

import scala.collection.mutable.ListBuffer
import bib.Bib._

class Schröder
{
    case class Node(fils: ListBuffer[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    def treeBuilder(n: Int): Schröder = //n -> Taille de l'arbre = nb Feuille
    {
        if (n <= 1) Leaf()
        else
        {
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

    def unrankTree(n: Int, s: Int): Schröder = //n -> Taille de l'arbre = nb Feuille, S -> Rank
    {
        if(n == 1) Leaf()
        else
        {
            var k = n - 1
            var gk = ComptageWeak(k)
            var r = s

            while(r >= 0)
            {
                r = r - combination(n - 1, k - 1) * gk
                k = k - 1
            }

            k = k + 1
            r = r + combination(n - 1, k - 1) * gk
            var s2 = r % gk
            var T = unrankTree(k, s2)
            var C = unrankComposition(n, k, r/gk)
        }
    }

    def unrankComposition(n: Int, k: Int, s: Int): ListBuffer[Int] =
    {
        if(n == 1 && k == 1 && s == 0) ListBuffer(1)
        else
        {
            var tmp = 0
            var s2 = s
            var C = ListBuffer[Int]()

            if(s2 < combination(n - 2, k - 1))
            {
                C = unrankComposition(n - 1, k, s2)
                C.update(C.size, C.last + 1)
                C
            }
            else
            {
                s2 = s2 - combination(n - 2, k - 1)
                C = unrankComposition(n - 1, k, s2):+ 1
                C
            }
        }
    }
}
