package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    (1 to r).product / ((1 to c).product * (1 to r - c).product)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def _balance(chars: List[Char], unbalBracks: Int): Boolean = {
      if (unbalBracks < 0) false
      else if (chars.isEmpty) {
        unbalBracks == 0
      }
      else if (chars.head == '(') {
        _balance(chars.tail, unbalBracks + 1)
      } else if (chars.head == ')') {
        _balance(chars.tail, unbalBracks - 1)
      } else {
        _balance(chars.tail, unbalBracks)
      }
    }

    _balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) return 0
    def _countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else _countChange(money, coins.tail) + _countChange(money - coins.head, coins)
    }
    _countChange(money, coins.distinct.sortWith(_ < _))
  }
}
