object ElpiTableauTest extends MagicTest() {
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.lf.elpi.ELPIExporter") // Register extension
  }

  override def run : Unit = {
    // hl("build MMT/LATIN2 mmt-omdoc") // Register extension
    hl("build MMT/LATIN2 lf-elpi logic") // Register extension
    hl("build MMT/LATIN2 lf-elpi fundamentals") // Register extension
  }
}
