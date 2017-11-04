/**
  * Decimal to Roman Numeral Converter for numbers less than 4000
  */
object Roman extends App {

  // While-loop Implementation
  def toRoman(input: Int): String = {
    if (input == 0 || input >= 4000) {
      return "Input must be between 1 and 4000"
    } else {
      val rom = "IVXLCDM"
      var pos = 0 // curr position in rom
      var n = input
      var outStr = ""

      while (n > 0) {
        val lastDig = n % 10
        var digRom = ""
        if (lastDig % 5 == 4) { // 4 or 9
          digRom += rom(pos).toString() + rom(pos + 1 + (lastDig / 5)).toString()
        } else { // 1,2,3 | 5 | 6,7,8
          digRom += rom.drop(pos + 1).take(1).toString() * (lastDig / 5) +
            rom(pos).toString() * (lastDig % 5)
        }
        n = n / 10
        pos += 2
        outStr = digRom + outStr
      }
      return outStr
    }
  }

  // Tail recursive implementation
  def toRomanTR(n: Int): String = {
    val rom = "IVXLCDM"
    def toRomanHelper(pos: Int, n: Int, str: String): String = {
      if (n == 0) {
        return str
      } else {
        val lastDig = n % 10
        if (lastDig % 5 == 4) { // 4 or 9
          val newStr = rom(pos).toString() + rom(pos + 1 + (lastDig / 5)).toString()
          toRomanHelper(pos + 2, n / 10, newStr + str)
        } else { // 1,2,3 | 5 | 6,7,8
          val newStr = rom.drop(pos + 1).take(1).toString() * (lastDig / 5) +
            rom(pos).toString() * (lastDig % 5)
          toRomanHelper(pos + 2, n / 10, newStr + str)
        }
      }
    }
    if (n == 0 || n >= 4000) return "Input must be between 1 and 4000"
    else return toRomanHelper(0, n, "")
  }
}

  /** Scala Nitpicks
  * 1. drop&take is used to avoid indexOutOfBound
  * 2. toString() is used to ensure that it doesn't convert to ASCII
  * /


