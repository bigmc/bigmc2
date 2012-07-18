package org.bigraph.bigmc

/** The parent trait for all edit script actions. **/
sealed trait EditAction { }
case class EditAddChild(child : Place, parent : Place) extends EditAction
case class EditRemove(child : Place) extends EditAction
case class EditConnect(link : Link, point : Link) extends EditAction
case class EditDisconnect(point : Link) extends EditAction

