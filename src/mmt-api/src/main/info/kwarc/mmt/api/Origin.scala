package info.kwarc.mmt.api

/** describes the origin of a generated knowledge item */
abstract class Origin
/** an original declaration */
case object Original extends Origin
/** obtained by elaborating a declaration */
case class ElaborationOf(source: GlobalName) extends Origin
/** obtained by elaborating the definition of a module*/
case object ElaborationOfDefinition extends Origin
/** generated by some agent */
case class GeneratedBy(by: AnyRef) extends Origin
/** materialized module */
case class Materialized(from: objects.Term) extends Origin

/** MRef left when taking a theory out of a document */
case object GeneratedMRef extends Origin

/** generated by lookup in partial links */
case object DefaultAssignment extends Origin
