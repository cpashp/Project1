class MyLexicalAnalyzer extends LexicalAnalyzer {


  var counter = 0
  var lexeme = ""

  override def addChar(): Unit = {
    lexeme = lexeme + Compiler.fileContents.charAt(counter - 1)
  }

  override def getChar(): Char = {
    val current = Compiler.fileContents.charAt(counter)
    counter = counter + 1
    return current
  }

  override def getNextToken(): Unit = {
    var c = getChar()
    var returnToken = ""
    while (CONSTANTS.whiteSpace.contains(c.toString())) {
      c = getChar()
    }

    c match {
      case '\\' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      case '#' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      case '*' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      case '+' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      case '[' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      case '!' => returnToken = processToken()
        lexeme =""
        if (lookup(returnToken)) {
          Compiler.currentToken = returnToken
        }
        else {
          println("Error")
          System.exit(1)
        }
      // call function to lex it
      case CONSTANTS.letters => Compiler.currentToken = processText()
        lexeme =""
      // default / error case
      case doh => println("DOH! Unexpected match")
    }

  }

  override def lookup(token : String): Boolean = {
  var result = false
  token match {
    case CONSTANTS.DOCB => result = true
    case CONSTANTS.DOCE => result = true
    case CONSTANTS.TITLEB => result = true
    case CONSTANTS.BRACKETE => result = true
    case CONSTANTS.HEADING => result = true
    case CONSTANTS.PARAB => result = true
    case CONSTANTS.PARAE => result = true
    case CONSTANTS.BOLD => result = true
    case CONSTANTS.LISTITEM => result = true
    case CONSTANTS.NEWLINE => result = true
    case CONSTANTS.LINKB => result = true
    case CONSTANTS.ADDRESSB => result = true
    case CONSTANTS.ADDRESSE => result = true
    case CONSTANTS.IMAGEB => result = true
    case CONSTANTS.DEFB => result = true
    case CONSTANTS.EQSIGN => result = true
    case CONSTANTS.USEB => result = true
  }
  return result
}
  def processText(): String= {
    addChar()
    var c = getChar()

    while(CONSTANTS.letters.contains(c.toString()))
      {
        addChar()
        c = getChar()
      }

    return lexeme
  }

  def processToken(): String= {
    addChar()

    val c = getChar()
      if(CONSTANTS.letters.contains(c.toString()))
      {
        lexeme= processText()
      }
      else
      processToken()
    return lexeme
  }

}
