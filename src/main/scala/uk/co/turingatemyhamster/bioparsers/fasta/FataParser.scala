package uk.co.turingatemyhamster.bioparsers
package fasta

import fastparse._

trait FastaParser[D, S] {

  val Gt = P(">")
  val Nl = P("\r\n" | "\r" | "\n")
  val NlEnd = P(Nl | End)

  def Desc: Parser[D]
  def Seq: Parser[S]
  def reduceSeq(ss: Seq[S]): S

  def DescLine = P(Gt ~ Desc ~ Nl)
  def SeqLine = P(!Gt ~ Seq ~ Nl)
  def SeqLastLine = P(!Gt ~ Seq ~ End)
  def SeqLines = P(SeqLine.rep ~ SeqLastLine.?) map
    { case (ls, lastO) => ls ++ lastO } map
    reduceSeq

  def Entry: Parser[Fasta[D, S]] = P(DescLine ~ SeqLines).map(Fasta.apply[D, S] _ tupled)

  def Entries = P(Entry.rep)

  def File = P(Start ~ Entries ~ End)
}

object FastaParser {

  type Fasta = fasta.Fasta[Description, String]

  def anySeq = FastaParser(SeqParser.AllParser)

  def dna = FastaParser(SeqParser.DnaRnaParser)

  def protein = FastaParser(SeqParser.ProteinParser)

  def apply(sp: SeqParser): FastaParser[Description, String] = {
    new FastaParser[Description, String] {
      override def Desc: P[Description] = DescriptionParser.Description

      override def reduceSeq(ss: Seq[String]): String = ss.mkString

      override def Seq: P[String] = sp.ResiduesWithSpaces
    }
  }
}

object DescriptionParser {

  def Nl = P("\n")
  def Sp = P(" ")

  def Ws = P(Sp.rep)

  def NonSpNl = P(!Sp ~ !Nl ~ AnyChar)

  def NonNl = P(!Nl ~ AnyChar)

  def Identifier = P(NonSpNl.rep.!).map(s => if(s.isEmpty) None else Some(s))
  def Comment = P(NonNl.rep.!).map(s => if(s.isEmpty) None else Some(s))

  def Description: Parser[Description] = P(Identifier ~ Ws ~ Comment).map(fasta.Description.apply _ tupled)
}

class SeqParser(residues: String) {

  def Sp = P(" ")

  def Residue = P(CharIn(residues.toLowerCase ++ residues.toUpperCase))
  def Residues: Parser[String] = P(Residue.rep.!)
  def Residues1Spaces0 = P(Residue.rep(1).! ~ Sp.rep)
  def Residues0Spaces1 = P(Residue.rep.! ~ Sp.rep(1))
  def ResiduesSpaces = P(Residues1Spaces0 | Residues0Spaces1)

  def ResiduesWithSpaces: Parser[String] = P(ResiduesSpaces.rep) map (_.mkString)
  
}

object SeqParser {
  def DnaStrictParser = new SeqParser("ACGT")
  def DnaNGapParser = new SeqParser("ACGTN.-")
  def DnaParser = new SeqParser("ACGTRYSWKMBDHVN.-")

  def RnaStrictParser = new SeqParser("ACGU")
  def RnaNGapParser = new SeqParser("ACGUN.-")
  def RnaParser = new SeqParser("ACGURYSWKMBDHVN.-")

  def DnaRnaParser = new SeqParser("ACGTURYSWKMBDHVN.-")

  def ProteinParser = new SeqParser("ABCDEFGHIKLMNPQRSTVWXYZ")

  def AllParser = new SeqParser("ABCDEFGHIJKLMNOPQRSTUVWXYZ._")
}
