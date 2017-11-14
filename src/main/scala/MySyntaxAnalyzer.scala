class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var parseTree = new scala.collection.mutable.Stack[String]

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parse tree / stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken() //LexicalAnalyzer.getNextToken()
      variableDefine()
      title()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        //add to parse tree / stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken() //get next token
      }
      else {
        println("Error")
        System.exit(1)
      }
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      innerText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Error")
        System.exit(1)
      }
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
      //push to stack
      parseTree.push(Compiler.currentToken)
    }
  }

  override def innerText(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
      //push to stack
      parseTree.push(Compiler.currentToken)
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
            //add to stack
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
              //add to stack
              parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
                //add to stack
                parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
            }
          }
        }
      }
    }
  }
  override def italics(): Unit = ???

  override def body(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
    }
    else {
      innerText()
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
      }
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
      {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB))
      {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            //add to stack
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else
            {
              System.out.println("Error")
              System.exit(1)
            }
        }
      }
    else
      {
        System.out.println("Error")
        System.exit(1)
      }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
            //add to stack
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
              //add to stack
              parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              variableDefine()
            }
          }
        }
      }
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
            //add to stack
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
              //add to stack
              parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
                //add to stack
                parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
            }
          }
        }
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
      }
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        parseTree.push(Compiler.currentToken)
      }
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      //add to stack
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }
  }

}