class MySyntaxAnalyzer extends SyntaxAnalyzer {

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parse tree / stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken() //LexicalAnalyzer.getNextToken()
      variableDefine()
      title()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        //add to parse tree / stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken() //get next token
      }
      else {
        println("Error - Must have \\END")
        System.exit(1)
      }
    }
    else {
      println("Error - Must have \\BEGIN")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      innerText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Error - Must have \\PARAE")
        System.exit(1)
      }
    }
    else {
      println("Error - Must have \\PARAB")
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
      Compiler.parseTree.push(Compiler.currentToken)
    }
    else {
      println("Error - inner item syntax not followed")
      System.exit(1)
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
      Compiler.parseTree.push(Compiler.currentToken)
    }
    else
      {
        println("Error - inner text syntax not followed")
        System.exit(1)
      }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
            //add to stack
            Compiler.parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
              //add to stack
              Compiler.parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
                //add to stack
                Compiler.parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else
                {
                  println("Error - missing )")
                  System.exit(1)
                }
            }
            else
              {
                println("Error - missing text")
                System.exit(1)
              }
          }
          else
          {
            println("Error - missing (")
            System.exit(1)
          }
        }
        else
        {
          println("Error - missing ]")
          System.exit(1)
        }
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing [")
      System.exit(1)
    }
  }

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
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else
        {
          println("Error - missing *")
          System.exit(1)
        }
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing *")
      System.exit(1)
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
      {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
    else
      {
        println("Error - newline missing")
        System.exit(1)
      }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB))
      {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            //add to stack
            Compiler.parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else
            {
              System.out.println("Error - missing ]")
              System.exit(1)
            }
        }
        else
        {
          println("Error - missing text")
          System.exit(1)
        }
      }
    else
      {
        System.out.println("Error - missing \\TITLE[")
        System.exit(1)
      }
  }

  override def variableDefine(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
            //add to stack
            Compiler.parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
              //add to stack
              Compiler.parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              variableDefine()
            }
            else
            {
              println("Error - missing ]")
              System.exit(1)
            }
          }
          else
          {
            println("Error - missing text")
            System.exit(1)
          }
        }
        else
        {
          println("Error - missing =")
          System.exit(1)
        }
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing \\DEF[")
      System.exit(1)
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
            //add to stack
            Compiler.parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
              //add to stack
              Compiler.parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
                //add to stack
                Compiler.parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else
              {
                println("Error - missing )")
                System.exit(1)
              }
            }
            else
            {
              println("Error - missing text")
              System.exit(1)
            }
          }
          else
          {
            println("Error - missing (")
            System.exit(1)
          }
        }
        else
        {
          println("Error - missing ]")
          System.exit(1)
        }
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing ![")
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          //add to stack
          Compiler.parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else
        {
          println("Error - missing ]")
          System.exit(1)
        }
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing \\USE[")
      System.exit(1)
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.validText.toString())) {
        //add to stack
        Compiler.parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Error - missing text")
        System.exit(1)
      }
    }
    else
    {
      println("Error - missing #")
      System.exit(1)
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      //add to stack
      Compiler.parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }
    else
    {
      println("Error - missing +")
      System.exit(1)
    }
  }

}