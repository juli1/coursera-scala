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
      if ( r <= 1) {
       return 1
      }
      else
        {
          if ((c == r ) || (c == 0)){
            return 1;
          }
          else
            {
              return pascal(c-1,r-1) + pascal(c,r-1)
            }
        }
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def accumulator(acc : Int, chars : List[Char]) : Boolean ={
      if (chars.isEmpty)
      {
        return acc == 0;
      }
      if (chars.head == '(') {
        return accumulator(acc + 1, chars.tail);
      }
      if (chars.head == ')')
      {
        if (acc == 0)
        {
          return false;
        }
        return accumulator(acc - 1, chars.tail);
      }
      return accumulator(acc, chars.tail);
    }
    accumulator(0, chars);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def accumulator (current: Int, target: Int, coins: List[Int]) : Int = {
      if (coins.isEmpty)
      {
        return 0
      }
      if (current > target)
      {
        return 0
      }
      if (current == target)
      {
        return 1
      }
      return (accumulator(current+coins.head, target, coins) +  accumulator(current, target, coins.tail))
    }
    accumulator(0, money, coins)
  }
}
