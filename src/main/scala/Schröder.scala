package schröder

import scala.collection.mutable.ListBuffer
import bib.Bib._

class Schröder()
{
    case class Node(fils: ListBuffer[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    def ajoutNewNode(k: Int, listLeaf: ListBuffer[Node], nb: Int, l: Int): (Schröder, ListBuffer[Node]) =
    {
        var index = 0
        var newNode = Leaf().asInstanceOf[Schröder]
        val list = ListBuffer[Leaf]()
        for(i <- 0 to nb) list.append(Leaf())

        var tmp = listLeaf.remove(k)

        for(j <- tmp.fils) j match
        {
            case Leaf() => newNode = j
            case _ =>
        }

        index = tmp.fils.indexOf(newNode)
        tmp.fils -= newNode
        tmp.fils.insert(index, Node(list, l))

        listLeaf.insert(k, this.asInstanceOf[Node])
        listLeaf.append(this.asInstanceOf[Node])

        (tmp.fils.apply(index), listLeaf)
    }

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
                    val res = ajoutNewNode(k, listLeaf, 2, l)
                    last = res._1
                    listLeaf = res._2
                    l += 1
                }
            }

            T
        }
    }

    def unrankTree(n: Int, s: Int): (Schröder, ListBuffer[Schröder]) = //n -> Taille de l'arbre = nb Feuille, S -> Rank
    {
        if(n == 1) (Leaf(), ListBuffer())
        else
        {
            var k = n - 1
            var gk = ComptageWeak(k)
            var r = s
            var l = 0

            while(r >= 0)
            {
                r = r - combination(n - 1, k - 1) * gk
                k = k - 1
                gk = ComptageWeak(k)
            }

            k = k + 1
            gk = ComptageWeak(k)
            r = r + combination(n - 1, k - 1) * gk

            var s2 = r % gk
            var (t, listLeaf) = unrankTree(k, s2)
            var C = unrankComposition(n, k, r/gk)

            t match
            {
                case Leaf() => l = 1
                case Node(_, label) => l = label + 1
            }

            for(i <- C)
            {
                if(i > 1)
                {

                }
            }

            (t, listLeaf)
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
            }
            else
            {
                s2 = s2 - combination(n - 2, k - 1)
                C = unrankComposition(n - 1, k - 1, s2):+ 1
            }

            C
        }
    }
}
