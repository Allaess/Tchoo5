package mw.tchoo.ecos

class EcosException(errNum: Int, errMsg: String) extends Exception(s"ECOS error $errNum: $errMsg")
object EcosException {
  def apply(errNum: Int, errMsg: String) = new EcosException(errNum, errMsg)
}