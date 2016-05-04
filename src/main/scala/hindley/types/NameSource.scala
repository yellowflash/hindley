package hindley.types

trait NameSource {
  def nextName:String
}

class LargeNameSource extends NameSource {
  var index = 0
  def nextName = {
    index = index + 1
    "_" + index
  }
}

class PrettyNameSource extends NameSource {
  var chara = 'a'.toInt - 1
  def nextName = {
    chara = chara + 1
    chara.toChar.toString
  }
}