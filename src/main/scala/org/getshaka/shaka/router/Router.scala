package org.getshaka.shaka.router

import org.getshaka.shaka.*
import org.scalajs.dom.*

import scala.collection.mutable.{Buffer, HashMap}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex

import scala.scalajs.js.URIUtils

object Router extends Component:
  private case class Route(regex: Regex, routing: Path => Component)

  private val PathState = useState(currentPath)
  private val Routes = Buffer.empty[Route]
  private val HashRegex = raw".*#(\S+)".r.anchored
  private val Origin =
    window.location.origin.getOrElse(window.location.protocol + "//" + window.location.host)

  private var root = "/"
  private var catchAllRouting: Path => Component = _

  document.body.addEventListener("click", e => handleClick(e))
  window.addEventListener("popstate", _ => updatePathState())

  override def template: Frag = Frag {
    PathState.bind { newPath =>

      val queryParams: Map[String, String] =
        URLSearchParams(window.location.search)
          .entries()
          .toIterator
          .map(t => (t._1, t._2))
          .toMap

      Routes.find(_.regex.matches(newPath)) match

        case Some(Route(regex, routing)) =>
          val pathParams = regex.findAllIn(newPath).toSeq
          routing(Path(pathParams, queryParams)).render

        case None =>
          if catchAllRouting != null then
            catchAllRouting(Path(Nil, queryParams)).render
    }
  }

  def root(root: String): Router.type =
    this.root = root
    this

  def catchAll(catchAllRouting: Path => Component): Router.type =
    this.catchAllRouting = catchAllRouting
    this

  def route(path: Regex, routing: Path => Component): Router.type =
    val frs = fullRegexString(root, path)
    Routes += Route(frs.r, routing)
    this

  private def currentPath: String =
    var uri = window.location.pathname.stripSuffix("/")
    val hash = window.location.hash
    if (hash.nonEmpty)
      uri += "/" + hash
    uri = URIUtils.decodeURI(uri)
    if uri.isEmpty then "/"
    else uri

  private def updatePathState(): Unit =
    PathState.setValue(currentPath)

  private def handleClick(e: MouseEvent): Unit =
    if e.defaultPrevented
      || e.button != 0
      || e.metaKey
      || e.ctrlKey
      || e.shiftKey
    then return
  
    val anchorOpt: Option[HTMLAnchorElement] =
      e.composedPath()
        .find(_.tagName.asInstanceOf[String|Unit] == "A")
        .map(_.asInstanceOf[HTMLAnchorElement])
    if anchorOpt.isEmpty then return
    val anchor = anchorOpt.get
  
    // https://github.com/lampepfl/dotty/issues/11632
    if
      (anchor.target != null && anchor.target.nonEmpty)
      || anchor.hasAttribute("download")
      || {
        val rel: String|Null = anchor.getAttribute("rel")
        rel != null && rel == "external"
      }
    then return
  
    val href = anchor.href
    if href == null
      || href.isEmpty
      || href.contains("mailto:")
      || !href.startsWith(Origin)
    then return
  
    e.preventDefault()
    if href != window.location.href then
      window.history.pushState(js.Object(), "", href)
      updatePathState()
  end handleClick

  private def fullRegexString(root: String, path: Regex): String =
    var fixedRoot = root
    if !root.startsWith("/") then fixedRoot = "/" + fixedRoot
    if !fixedRoot.endsWith("/") then fixedRoot += "/"
    // ends with a non-capturing group to allow hashes
    "^" + fixedRoot + path.unanchored.regex.stripPrefix("/").stripSuffix("/") + "(?:/#.*)?"

end Router

// todo add composedPath to scalajs-dom
@js.native
private trait MouseEvent extends js.Object:
  val defaultPrevented: Boolean = js.native
  val button: Int = js.native
  val metaKey: Boolean = js.native
  val ctrlKey: Boolean = js.native
  val shiftKey: Boolean = js.native
  def composedPath(): js.Array[js.Dynamic] = js.native
  def preventDefault(): Unit = js.native
