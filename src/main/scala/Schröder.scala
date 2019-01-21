package schröder

import scala.collection.mutable.ListBuffer
import bib.Bib._

class Schröder()
{
    case class Node(fils: ListBuffer[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    def whichLeaf(k: Int, listLeaf:ListBuffer[Schröder]): Schröder =
    {
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
                else i = index + 1
            }
            father.fils.apply(index)
        }
    }

    def ajoutNewNode(k: Int, listLeaf: ListBuffer[Schröder], nb: Int, l: Int): (Schröder, ListBuffer[Schröder]) =
    {
        var index = 0
        var leafToDestroy = Leaf().asInstanceOf[Schröder]
        val list = ListBuffer[Schröder]()
        for(i <- 0 to nb) list.append(Leaf())

        leafToDestroy = whichLeaf(k, listLeaf)

        var tmp = listLeaf.remove(k).asInstanceOf[Node]

        val newNode = Node(list, l)

        index = tmp.fils.indexOf(leafToDestroy)
        tmp.fils -= leafToDestroy
        tmp.fils.insert(index, newNode)

        listLeaf.insert(k, newNode)
        listLeaf.append(newNode)

        (newNode, listLeaf)
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
                    val (res1, res2) = T.ajoutNewNode(k, listLeaf, 2, l)
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
                C.update(C.size, C.last + 1)

            }
            else
            {
                s2 = s2 - combination(n - 2, k - 1)
                C = unrankComposition(n - 1, k - 1, s2):+ 1
            }
            C
        }
      C
    }

  def unrankTreeStrong(n: Int, s: Int, i:Int): (Schröder, ListBuffer[Schröder]) = //n -> Taille de l'arbre = nb Feuille, S -> Rank,i -> nb de noeud interne
  {
    if(n == 1) (Leaf(), ListBuffer())
    else
    {
      var k = n - 1
      var tk = ComptageStrong(n,i)
      var r = s
      var l = 0

      while(r >= 0)
      {
        r = r - combination(n - 1, k - 1) * tk
        k = k - 1
        tk = ComptageStrong(n,i)
      }

      k = k + 1
      tk = ComptageStrong(n,i)
      r = r + combination(n - 1, k - 1) * tk

      var s2 = r % tk
      var (t, listLeaf) = unrankTreeStrong(k, s2, i)

      t match
      {
        case Leaf() => l = 1
        case Node(_, label) => l = label + 1
      }


      (t, listLeaf)
    }
  }
}
