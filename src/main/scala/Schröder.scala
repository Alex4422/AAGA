package schröder

import scala.collection.mutable.ListBuffer
import bib.Bib._

class Schröder()
{
    case class Node(fils: ListBuffer[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    def nbNoeudInterne(): Int = this match
    {
        case Node(fils, _) => this._nbNoeudInterne(0)
        case _ => 0
    }

    def _nbNoeudInterne(n: Int): Int = this match
    {
        case Node(fils, _) =>
        {
            var res = n
            for(e <- fils) res += e._nbNoeudInterne(n)
            res
        }
        case _ => n
    }

    /*def hauteur(): Int =
    {

    }*/

    def whichLeaf(k: Int, listLeaf:ListBuffer[Schröder]): Schröder =
    {
        println("HEY")
        var compteur = 0
        var i = 0
        var index = 0
        var continue = true
        val father = listLeaf.apply(k).asInstanceOf[Node]
        var nbSon = listLeaf.count(_ == father)

        if(nbSon == 1) father.fils.apply(0)
        else
        {
            while(continue)
            {
                index = listLeaf.indexOf(father, i)
                if(index == k) continue = false
                else
                {
                    i = index + 1
                    compteur = compteur + 1
                }
            }
            father.fils.apply(compteur)
        }
    }

    def ajoutNewNode(k: Int, listLeaf: ListBuffer[Schröder], nb: Int, l: Int): (Schröder, ListBuffer[Schröder]) =
    {
        var index = 0
        var leafToDestroy = Leaf().asInstanceOf[Schröder]
        val list = ListBuffer[Schröder]()
        for(i <- 0 to nb - 1) list.append(Leaf())

        if(listLeaf.isEmpty)
        {
            println("HEY3")
            val newNode = Node(list, 0)
            val list2 = ListBuffer[Schröder]()
            for(i <- 0 to nb) list2.append(newNode)
            (newNode, list2)
        }
        else
        {
            println("HEY2")
            leafToDestroy = whichLeaf(k, listLeaf)

            var tmp = listLeaf.remove(k).asInstanceOf[Node]

            val newNode = Node(list, l)

            index = tmp.fils.indexOf(leafToDestroy)
            println(tmp)
            tmp.fils -= leafToDestroy
            println(tmp)
            tmp.fils.insert(index, newNode)
            println(tmp)

            listLeaf.insert(k, newNode)
            listLeaf.append(newNode)

            (newNode, listLeaf)
        }
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


            var listLeaf = ListBuffer[Schröder](T, T)

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
                    val (res1, res2) = ajoutNewNode(k, listLeaf, 2, l)
                    last = res1.asInstanceOf[Node]
                    listLeaf = res2
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

            for(i <- 0 to C.size - 1)
            {
                if(C.apply(i) > 1)
                {
                    var res = ajoutNewNode(i, listLeaf, C.apply(i), 0)
                    listLeaf = res._2
                }
            }

            (t, listLeaf)
        }
    }

    def unrankComposition(n: Int, k: Int, s: Int): ListBuffer[Int] =
    {
        var C = ListBuffer[Int]()

        if(n == 1 && k == 1 && s == 0) C.insert(0,1)

        if(n == k)
        {
            for(i <- 0 to k-1)C.insert(i,1)
        }

        else
        {
            var tmp = 0
            var s2 = s

            if(s2 < combination(n - 2, k - 1))
            {
                C = unrankComposition(n - 1, k, s2)
                C.update(C.size - 1, C.last + 1)

            }
            else
            {
                s2 = s2 - combination(n - 2, k - 1)
                C = unrankComposition(n - 1, k - 1, s2):+ 1
            }
        }
      C
    }

    def unrankTreeStrong(n: Int, s: Int): (Schröder, ListBuffer[Schröder], Int) = //n -> Taille de l'arbre = nb Feuille, S -> Rank
    {
        if(n == 1) (Leaf(), ListBuffer(), 0)
        else
        {
            val random = scala.util.Random
            var tmp = Node(ListBuffer(Leaf(), Leaf()), 1)
            var t2 = tmp
            var rand = 0
            var k = n - 1
            var tk = ComptageStrong(n)
            var r = s

            while(r >= 0)
            {
                r = r - combination(n - 1, k - 1) * tk
                k = k - 1
                tk = ComptageStrong(n)
            }

            k = k + 1
            tk = ComptageStrong(n)
            r = r + combination(n - 1, k - 1) * tk

            var s2 = r % tk
            var (t, listLeaf, l) = unrankTreeStrong(k, s2)

            t match
            {
                case Leaf() =>
                {
                    return (tmp, ListBuffer(tmp, tmp), 1)
                }
                case _ =>
            }

            t2 = t.asInstanceOf[Node]

            rand = random.nextInt(listLeaf.size + 1)
            println("rand : "+rand)
            println("size : "+listLeaf.size)

            if(rand == listLeaf.size)
            {
                listLeaf.append(t)
                t2.fils.append(Leaf())
                (t2, listLeaf, l)
            }
            else
            {
                println("COUCOU")
                val (res1, res2) = ajoutNewNode(rand, listLeaf, 2, l + 1)
                (t2, res2, l + 1)
            }
        }
    }
}
