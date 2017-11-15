import java.awt.Desktop
import java.io.{File, IOException}

object Compiler {

  var currentToken: String = ""
  var fileContents: String = ""
  var parseTree = new scala.collection.mutable.Stack[String]

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  var fileName = Array[String]()
  var newfileName: String = ""
  var newFile = File

  def main(args: Array[String]): Unit = {

    checkFile(args)
    fileName = args
    readFile(args(0))

    Scanner.getNextToken()
    Parser.gittex()

    newfileName = fileName.toString().replaceAll(".gtx", ".html")
    newFile= new File(newfileName)

    SemanticAnalyzer.translate()

    openHTMLFileInBrowser(newfileName)

  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1){
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
