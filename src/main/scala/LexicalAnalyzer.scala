trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup(token: String) : Boolean = {
    var result = false
    token match {
      case CONSTANTS.DOCB => result =true
      case CONSTANTS.DOCE => result =true
      case CONSTANTS.TITLEB => result =true
      case CONSTANTS.BRACKETE => result =true
      case CONSTANTS.HEADING => result =true
      case CONSTANTS.PARAB => result =true
      case CONSTANTS.PARAE => result =true
      case CONSTANTS.BOLD => result =true
      case CONSTANTS.LISTITEM => result =true
      case CONSTANTS.NEWLINE => result =true
      case CONSTANTS.LINKB => result =true
      case CONSTANTS.ADDRESSB => result =true
      case CONSTANTS.ADDRESSE => result =true
      case CONSTANTS.IMAGEB => result =true
      case CONSTANTS.DEFB => result =true
      case CONSTANTS.EQSIGN => result =true
      case CONSTANTS.USEB => result =true
    }
    return result
  }
}
