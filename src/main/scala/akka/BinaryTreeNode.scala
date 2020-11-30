package akka

import akka.BinaryTreeSet.{Operation, OperationReply}
import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove => doRemove(remove)
  }

  private def getPosition(m: Operation, elem: Int): Position = {
    if(m.elem < elem) Left else Right
  }

  private def nextNodeOrReply(m: Operation, reply: OperationReply): Unit = {
    subtrees.get(getPosition(m,elem)) match {
      case Some(node) => node ! m
      case None =>  m.requester ! reply
    }
  }

  private def createNode(m: Insert): OperationReply = {
    val node = context.actorOf(props(m.elem, false))
    subtrees = subtrees updated (getPosition(m,elem), node)
    OperationFinished(m.id)
  }

  private def removeNode(m: Remove): OperationReply = {
    removed = true
    OperationFinished(m.id)
  }

  private def doInsert(m: Insert): Unit = {
    if(m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else {
      nextNodeOrReply(m, createNode(m))
    }
  }

  private def doContains(m: Contains): Unit = {
    if(m.elem == elem) {
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      nextNodeOrReply(m, ContainsResult(m.id, false))
    }
  }

  private def doRemove(m: Remove): Unit = {
    if(m.elem == elem) {
      m.requester ! removeNode(m)
    } else {
      nextNodeOrReply(m, removeNode(m))
    }
  }
}
