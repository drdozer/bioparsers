package uk.co.turingatemyhamster.bioparsers
package swissprot

import fastparse._

class SwissprotParser {

  implicit class StringP(val s: String) {
    def --- [T](p: Parser[T]): Parser[T] = P(s ~ "   " ~ p ~ Nl)
  }

  def Semicolon = P(";")
  def Period = P(".")
  def Coma = ","
  def Nl = P("\n")
  def Sp = P(" ")
  def Hyphen = P("-")
  def Ws = P(Sp.rep)
  def Underscore = P("_")
  def Digit = P(CharIn("0123456789"))
  def Digits = P(Digit.rep)
  def Letter = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  def LetterOrUnderscore = P(Letter | Underscore)
  def LetterOrHyphen = P(Letter | Hyphen)
  def Letters = P(Letter.rep)

  def LetterOrUnderscores = P(LetterOrUnderscore.rep.!)
  def LetterOrHyphens = P(LetterOrHyphen.rep.!)
  def Integer = P(Digits.!) map (_.toInt)
  def Integer2 = P((Digit ~ Digit).!) map (_.toInt)
  def Integer4 = P((Digit ~ Digit ~ Digit ~ Digit).!) map (_.toInt)
  def Letter3 = P((Letter ~ Letter ~ Letter).!)

  def Identifier = P(LetterOrUnderscore.rep(1).!)

  def EntryEnd = P("//")


  def Reviewed = P("Reviewed").map(_ => swissprot.Reviewed)
  def Unreviewed = P("Unreviewed").map(_ => swissprot.Unreviewed)
  def Status = P(Reviewed | Unreviewed)

  def AA = P("AA")

  def IDContent = P(Identifier ~ Ws ~ Status ~ Semicolon ~ Ws ~ Integer ~ Ws ~ AA ~ Period)
  def ID = ("ID" --- IDContent) map (swissprot.ID.apply _ tupled)

  def Accession = P(Identifier ~ Semicolon)
  def Accessions = P(Accession.rep(sep = Ws))

  def AC = P(("AC" --- Accessions).rep) map (accs => swissprot.AC(accs.flatten))


  def SPDate = P(Integer2 ~ Letter3 ~ Integer4) map (swissprot.SPDate.apply _ tupled)
  def IntegrationDate = P(SPDate ~ Coma ~ Ws ~ "integrated into UniProtKB/" ~ LetterOrHyphens ~ Period) map
    (swissprot.IntegrationDate.apply _ tupled)
  def SequenceVDate = P(SPDate ~ Coma ~ Ws ~ "sequence version" ~ Ws ~ Integer ~ Period) map
    (swissprot.SequenceVDate.apply _ tupled)
  def EntryVDate = P(SPDate ~ Coma ~ Ws ~ "entry version" ~ Ws ~ Integer ~ Period) map
    (swissprot.EntryVDate.apply _ tupled)

  def DT = P(
    ("DT" --- IntegrationDate) ~
      ("DT" --- SequenceVDate) ~
      ("DT" --- EntryVDate))


}
