import java.io.FileWriter
import java.io.BufferedWriter
class MySemanticAnalyzer {

  var variable = new VarDefNode("", "")

  val bw = new BufferedWriter(new FileWriter(Compiler.newFile))


  def translate() = {
    Compiler.parseTree.reverse.foreach { (s: String) =>
      caseswitch(s)
      Compiler.parseTree.pop()
    }
    bw.close()
  }
  def caseswitch(s : String) = {
    s match {
      case "\\BEGIN" => bw.write("<html>")
      case "\\TITLE[" => bw.write("<head>")
                          bw.write("<title>")
                          Compiler.parseTree.pop()
                          bw.write(Compiler.currentToken)
                          bw.write("</title>")
                          bw.write("</head")
      case "#" => bw.write("<h1>")
                  Compiler.parseTree.pop()
                  bw.write(Compiler.currentToken)
                  bw.write("</h1>")
      case "\\PARAB" => bw.write("<p>")
      case "\\PARAE" => bw.write("</p>")
      case "*" => bw.write("<b>")
      case "[" => bw.write("<a href =")
                  Compiler.parseTree.pop()
                  val linktitle = Compiler.currentToken
                  Compiler.parseTree.pop()
                  Compiler.parseTree.pop()
                  Compiler.parseTree.pop()
                  bw.write('"'+Compiler.currentToken+'"'+">")
                  bw.write(linktitle+"</a>")
      case "![" => bw.write("<img src=")
                  Compiler.parseTree.pop()
                  val imagetitle = Compiler.currentToken
                  Compiler.parseTree.pop()
                  Compiler.parseTree.pop()
                  Compiler.parseTree.pop()
                  bw.write('"'+Compiler.currentToken+'"'+ " alt="+'"'+imagetitle+'"'+">")
      case "+" => bw.write("<li>")
      case "\\\\" => bw.write("<br>")
      case "\\END" => bw.write("</html>")
      case "\\Def[" => Compiler.parseTree.pop()
                      val name = Compiler.currentToken
                      Compiler.parseTree.pop()
                      Compiler.parseTree.pop()
                      val value = Compiler.currentToken
                      var variable = new VarDefNode(name, value)
                      Compiler.parseTree.pop()
      case "\\USE[" => Compiler.parseTree.pop()
                        bw.write(variable.valueOut())
      case CONSTANTS.validText => bw.write(variable.valueOut())
    }
  }
}
