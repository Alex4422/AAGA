import main._

class Schröder
{
    case class Node(fils: List[Schröder], value: Int) extends Schröder
    case class Leaf() extends Schröder
    case class Void() extends Schröder

    /*def treeBuilder(n: Int): Schröder = //n -> Taille de l'arbre = nb Feuille
    {
        if (n <= 1) Leaf()

        val rand = scala.util.Random
        var T = Node(List(Leaf, Leaf), 1)
        var last = T
        var l = 2
        var k = 0

        for(i <- 3 to n)
        {
            k = random.nextInt(i - 1) + 1
            if(k == i) last.fils::Leaf

        }
    }*/
}
