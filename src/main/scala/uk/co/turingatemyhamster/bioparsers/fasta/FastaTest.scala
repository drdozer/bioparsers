package uk.co.turingatemyhamster.bioparsers.fasta

object FastaTest {
  def main(args: Array[String]): Unit = {

    println("Test parsing description text")

    println(DescriptionParser.Description.parse("foo bar baz"))
    println(DescriptionParser.Description.parse("foo bar"))
    println(DescriptionParser.Description.parse("foo"))
    println(DescriptionParser.Description.parse(""))

    println(DescriptionParser.Description.parse(
      """foo bar baz
        |
      """.stripMargin))

    println(DescriptionParser.Description.parse(
      """foo bar
        |
      """.stripMargin
    ))

    println(DescriptionParser.Description.parse(
      """foo
        |
      """.stripMargin
    ))

    println(DescriptionParser.Description.parse(
      """
        |
      """.stripMargin
    ))

    val fa = FastaParser.anySeq

    println("Test parsing description line")
    println(fa.DescLine.parse(
      """>foo bar baz
        |
      """.stripMargin))

    println(fa.DescLine.parse(
      """>foo bar
        |
      """.stripMargin
    ))

    println(fa.DescLine.parse(
      """>foo
        |
      """.stripMargin
    ))

    println(fa.DescLine.parse(
      """>
        |
      """.stripMargin
    ))

    val seq = """AGGKLDSDSLKJGLKJDFGADG
                |LKSDFLKDJLAKSJFDLAKJDFL
                |OJIGONRANPVNPVNEPNVENE
                |
              """.stripMargin

    val fseq = """>foo bar baz
                 |AGGKLDSDSLKJGLKJDFGADG
                 |LKSDFLKDJLAKSJFDLAKJDFL
                 |OJIGONRANPVNPVNEPNVENE
                 |>
                 |IOJAORJARARNOBNLBNLALNB
               """.stripMargin

    val sp = SeqParser.AllParser

    println("Test parsing sequence")
    println(sp.Residue.parse(seq))
    println(sp.Residues.parse(seq))

    println(fa.SeqLine.parse(seq))
    println(fa.Entries.parse(fseq))

//    println("Test parsing sequence line")
//    println(fa.SeqLine.parse(
//      ))
  }
}