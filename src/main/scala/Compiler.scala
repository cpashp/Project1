object Compiler {

  var currentToken: String = ""
  var fileContents: String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()
  }

}
