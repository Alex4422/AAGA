package bib

object Bib
{
    def combination(n: Int, k: Int): Int =
    {
        if (k == 0 || k == n) 1
        else combination(n - 1, k - 1) + combination(n - 1, k)
    }

    def ComptageStrong(n: Int): Int = //n -> Feuille
    {
        if(n <= 2) 1
        else n * ComptageStrong(n-1)
    }

    def ComptageWeak(n: Int): Int =
    {
        if(n <= 0) 0
        else if(n == 1) 1
        else
        {
            var res = 0
            for(k <- 1 to (n - 1)) res = combination(n - 1, k - 1) * ComptageWeak(k) + res
            res
        }
    }
}
