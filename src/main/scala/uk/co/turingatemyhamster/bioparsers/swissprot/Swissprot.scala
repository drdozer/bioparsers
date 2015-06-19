package uk.co.turingatemyhamster.bioparsers.swissprot

case class Swissprot(blocks: Seq[TaggedBlock])

trait Status
case object Reviewed extends Status
case object Unreviewed extends Status

case class SPDate(dd: Int, mmm: String, yyyy: Int)

trait TaggedBlock

case class ID(entityName: String, status: Status, sequenceLength: Int) extends TaggedBlock


case class AC(accessions: Seq[String]) extends TaggedBlock


case class IntegrationDate(date: SPDate, databaseName: String)

case class SequenceVDate(date: SPDate, sequenceVersion: Int)

case class EntryVDate(Date: SPDate, entryVersion: Int)

case class DT(integration: IntegrationDate, sequenceV: SequenceVDate, entryV: EntryVDate) extends TaggedBlock


case class RecName()
case class AltName()
case class SubName()
case class DE() extends TaggedBlock

case class GN() extends TaggedBlock
case class OS() extends TaggedBlock
case class OX() extends TaggedBlock
case class OH() extends TaggedBlock
case class RN() extends TaggedBlock
case class RP() extends TaggedBlock
case class RC() extends TaggedBlock
case class RX() extends TaggedBlock
case class RG() extends TaggedBlock
case class RA() extends TaggedBlock
case class RT() extends TaggedBlock
case class RL() extends TaggedBlock
case class CC() extends TaggedBlock
case class DR() extends TaggedBlock
case class PE() extends TaggedBlock
case class KW() extends TaggedBlock
case class FT() extends TaggedBlock
case class SQ() extends TaggedBlock
case class Sequence() extends TaggedBlock
