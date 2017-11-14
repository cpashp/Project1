class MyLexicalAnalyzer extends LexicalAnalyzer{

  //var iterator = Iterator(Compiler.fileContents.toCharArray())
  val max = Compiler.fileContents.length()
  var counter =0
  var lexeme = ""
  override def addChar(): Unit = {
    lexeme = lexeme + Compiler.fileContents.charAt(counter-1)
  }

  override def getChar(): Char = {
        val current = Compiler.fileContents.charAt(counter)
        counter = counter+1
        return current
  }

  override def getNextToken(): Unit = {
    var c = getChar()
    var returnToken = ""
    while (c==CONSTANTS.whiteSpace)
      {
        c = getChar()
      }

    c match {
      case '\\' => returnToken =processToken('\\')
        if(lookup(returnToken) == true) {
        Compiler.currentToken = returnToken
      }
      else
        {
          println("Error")
          System.exit(1)
        }
      case '#' => returnToken =processToken('#')
        if(lookup(returnToken) == true) {
        Compiler.currentToken = returnToken
      }
      else
      {
        println("Error")
        System.exit(1)
      }
      case '*' => returnToken =processToken('*')
        if(lookup(returnToken) == true) {
        Compiler.currentToken = returnToken
      }
      else
      {
        println("Error")
        System.exit(1)
      }
      case '+' => returnToken =processToken('+')
        if(lookup(returnToken) == true) {
        Compiler.currentToken = returnToken
      }
      else
      {
        println("Error")
        System.exit(1)
      }
      case '[' => returnToken =processToken('[')
        if(lookup(returnToken) == true) {
        Compiler.currentToken = returnToken
      }
      else
      {
        println("Error")
        System.exit(1)
      }
      case '!' => returnToken =processToken('!')
        if(lookup(returnToken) == true) {
          Compiler.currentToken = returnToken
        }
        else
        {
          println("Error")
          System.exit(1)
        }
        // call function to lex it
      case CONSTANTS.letters => Compiler.currentToken = processText()
      // default / error case
      case doh => println("DOH! Unexpected match")
    }

  }

   //override def lookup(): Boolean = ???
  def processText(): String= {
    lexeme = ""
    addChar()
    var c = getChar()

    while(c == CONSTANTS.letters)
      {
        addChar()
        c = getChar()
      }

    return lexeme
  }

  def processToken(character : Char): String= {
    lexeme = ""
    addChar()
    if (character =='\\')
      {

      }
    var c = getChar()
    if(c==CONSTANTS.letters)
      {

      }
    return lexeme
  }


}
