package uk.co.turingatemyhamster.bioparsers.fasta

/**
 * Created by nmrp3 on 16/06/15.
 */
case class Fasta[D, S](desc: D, seq: S)

case class Description(id: Option[String], comment: Option[String])
