package org.random.musicbandrel.comet

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
import java.lang.StringBuilder
import java.net.URLEncoder
import scala.collection.mutable.Set
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.util.EntityUtils
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.CometActor
import net.liftweb.http.SHtml
import net.liftweb.json.JsonParser._
import net.liftweb.util.ActorPing
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.utils.URLEncodedUtils

//import java.util.List

class MemberQuery extends CometActor {
  var artistId: String = ""
  var artist: String = ""

  var cytoscapeJsonTemplate = """ {
    "dataSchema": { "nodes": [{ "name": "label", "type": "string" }, { "name": "type", "type": "string" }], "edges": [{ "name": "label", "type": "string" }] },
    "data": { "nodes": [%s], "edges": [%s] }
  } """

  var cytoscapeNodeTemplate = """{ "id" : "%s", "label": "%s", "type": "%s" }"""
  var cytoscapeEdgeTemplate = """{ "id" : "%s", "label": "%s", "source": "%s", "target": "%s", "directed": true }"""

  def queryFreeBase(query: String): String = {
    //val urlAddr = String.format("""http://api.freebase.com/api/service/mqlread?query=%s""", URLEncoder.encode(query, "UTF-8"))
    
    var params = new java.util.ArrayList[BasicNameValuePair]()
    params.add(new BasicNameValuePair("query", query))
    params.add(new BasicNameValuePair("key", "AIzaSyBRp21uuUB5_9GSnExpzqk_1WXP5L5VpRw"));
    
    val urlAddr = """http://www.freebase.com/ajax/144a.lib.www.tags.svn.freebase-site.googlecode.dev/cuecard/mqlread.ajax"""
    //val urlAddr = """https://www.googleapis.com/freebase/v1/search"""
    val url = urlAddr + "?" + URLEncodedUtils.format(params, "UTF-8");  

    var httpclient = new DefaultHttpClient();

    var httpget = new HttpGet(url);
    var response = httpclient.execute(httpget);
    var json: StringBuilder = new StringBuilder();
    EntityUtils.toString(response.getEntity())
  }

  def getFreeBaseUrl(query: String) {
  }

  case class Member(id: String, name: String);
  case class Membership(member: List[Member]);
  case class MemberJson(result: List[Membership]);
  case class MemberJsonResult(result: MemberJson);

  def getMembers(artistId: String): Map[String, String] = {
    var members = Map[String, String]()
    var member_ids = Set[String]()
    val queryText = String.format("""{"query":[{
    	"type" : "/music/group_membership", 
        "member" : [{"id" : null, "name" : null, "type" : "/music/artist" }],
        "group" : [{"id" : "%s" }]
    }]}""", artistId)

    val json = queryFreeBase(queryText)

    implicit val formats = net.liftweb.json.DefaultFormats
    val parsedObj = parse(json)
    val jsonObj = parsedObj.extract[MemberJsonResult]

    jsonObj.result.result.foreach(membership => {
      val member = membership.member(0)
      val member_id = member.id
      val member_name = member.name.replaceAll("\"", "\'")
      if (!member_ids.contains(member_id)) {
        member_ids += member_id
        members += ((member_id, member_name))
      }
    })

    members
  }

  case class Group(id: String, name: String);
  case class GroupMembership(group: List[Group]);
  case class GroupJson(result: List[GroupMembership]);
  case class GroupJsonResult(result: GroupJson);

  def getRelatedGroups(memberId: String): Map[String, String] = {
    var groups = Map[String, String]()
    val queryText = String.format("""{"query":[{
        "type":"/music/group_membership",
        "group":[{"id" : null, "name" : null}],"member":[{"id":"%s"}]
    }]} """, memberId)

    val json = queryFreeBase(queryText)

    implicit val formats = net.liftweb.json.DefaultFormats
    val jsonObj = parse(json).extract[GroupJsonResult]

    if (jsonObj != null && jsonObj.result != null) {
      jsonObj.result.result.foreach(membership => {
        if (membership.group != null && membership.group.size > 0) {
          val group = membership.group(0)
          if (group != null && group.name != null) {
            val group_name = group.name.replaceAll("\"", "\'").replaceAll("&amp;", "&")
            if (!group_name.equals(artist)) {
              groups += ((group.id, group_name))
            }
          }
        }
      })
    }
    groups
  }

  def buildMembersJson(artist: String, members: Map[String, String]): (String, StringBuilder, StringBuilder) = {
    val jsonNodes = new StringBuilder()
    val jsonEdges = new StringBuilder()
    val artistNode = cytoscapeNodeTemplate.format(artistId, artist, "artist");
    jsonNodes.append(artistNode)
    members.foreach((member) => {
      val memberId = member._1
      val memberName = member._2
      val memberNode = cytoscapeNodeTemplate.format(memberId, memberName, "member");
      jsonNodes.append(',').append(memberNode)
      if (jsonEdges.length() > 0) {
        jsonEdges.append(',')
      }
      val memberEdge = cytoscapeEdgeTemplate.format(memberId, memberId, artistId, memberId);
      jsonEdges.append(memberEdge)
    })
    (cytoscapeJsonTemplate.format(jsonNodes, jsonEdges), jsonNodes, jsonEdges)
  }

  def addGroupsToJson(memberId: String, groups: Map[String, String],
    jsonNodes: StringBuilder, jsonEdges: StringBuilder, total_groups: Set[String]): Unit = {
    groups.foreach((group) => {
      val groupId = group._1
      val groupName = group._2
      if (!artist.equals(groupName)) {
        if (!total_groups.contains(groupId)) {
          val groupNode = cytoscapeNodeTemplate.format(groupId, groupName, "relatedgroup");
          jsonNodes.append(',').append(groupNode)
          total_groups += groupId
        }
        if (jsonEdges.length() > 0) {
          jsonEdges.append(',')
        }
        val memberEdge = cytoscapeEdgeTemplate.format(memberId, memberId, memberId, groupId);
        jsonEdges.append(memberEdge)
      }
    })
  }

  def pushMembers(): Unit = {
    val members = getMembers(artistId);
    val (membersJson, jsonNodes, jsonEdges) = buildMembersJson(artist, members)
    ActorPing.schedule(this, membersJson, 0L)
    val total_groups = Set[String]()
    members.foreach { (member) =>
      val groups = getRelatedGroups(member._1)
      addGroupsToJson(member._1, groups, jsonNodes, jsonEdges, total_groups)
    }
    val totalJson = cytoscapeJsonTemplate.format(jsonNodes, jsonEdges)
    ActorPing.schedule(this, totalJson, 5000L)
  }

  /**
   * The render method in this case returns a function
   * that transforms NodeSeq => NodeSeq.  In this case,
   * the function transforms a form input element by attaching
   * behavior to the submit button.
   */
  def render = {
    bind("example",
      "artist" -> SHtml.text("", artist = _, "id" -> "artist"),
      "artist_id" -> SHtml.hidden(artistId = _, "", "id" -> "artist_id"),
      "submit" -> SHtml.ajaxSubmit("Submit", { () => pushMembers }, "id" -> "submit"))
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    case partialJson: String => {
      var jsonObj = parse(partialJson)
      var crCmd = JsCrVar("partialJson", jsonObj)
      partialUpdate(crCmd & JsRaw("vis_draw()").cmd)
    }
  }

}