package org.random.musicbandrel.snippet

/**
 * A snippet transforms input to output... it transforms
 * templates to dynamic content.  Lift's templates can invoke
 * snippets and the snippets are resolved in many different
 * ways including "by convention".  The snippet package
 * has named snippets and those snippets can be classes
 * that are instantiated when invoked or they can be
 * objects, singletons.  Singletons are useful if there's
 * no explicit state managed in the snippet.
 */
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml

object ClearInput {
  /**
   * The render method in this case returns a function
   * that transforms NodeSeq => NodeSeq.  In this case,
   * the function transforms a form input element by attaching
   * behavior to the input.  The behavior is to send a message
   * to the ChatServer and then returns JavaScript which
   * clears the input.
   */
  def render = SHtml.onSubmit(artistId => {
    //ArtistServer ! artistId
    SetValById("artist", "")
//    getMembers(artistId);
//    val sb = new StringBuilder()
//    for (member <- members) {
//    	sb.addString(new StringBuilder(member + "\n") )
//    }
//    SetValById("members", sb.toString())
  })
}