package org.getshaka.shaka.router

import org.getshaka.shaka.*
import org.scalajs.dom.*

import scala.collection.mutable.{Buffer, HashMap}
import scala.collection.Seq
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex

import scala.scalajs.js.URIUtils

private case class Route(regex: Regex, component: Component, paramStates: Seq[OpenState[String]])

class Router(root: String = "/") extends Component:
  private val routes = Buffer.empty[Route]
  private var catchAll: Component = null

  def route(path: Regex, component: Component): Router =
    routes += Router.registerRoute(root, path, component)
    this

  def route(routableComponent: Routable & Component): Router =
    routes += Router.registerRoute(root, routableComponent.path, routableComponent)
    this

  def catchAll(component: Component): Router =
    catchAll = component
    this

  override def template: Frag = Frag {
    Router.PathState.bind(newPath =>
      routes.find(_.regex.matches(newPath)) match
        case Some(Route(regex, component, paramStates)) =>
          component.render
          for (param, paramState) <- regex.findAllIn(newPath).zip(paramStates) do
              paramState.setValue(param)
          newPath match
            case Router.HashRegex(hashId) =>
              val elementToScroll = document.getElementById(hashId).asInstanceOf[Element|Null]
              if elementToScroll != null then elementToScroll.asInstanceOf[js.Dynamic].scrollIntoView()
              else window.scrollTo(0, 0)
            case _ =>
              window.scrollTo(0, 0)
        case None =>
          if catchAll != null then catchAll.render
    )
  }

object Router:
  private val PathState = useState(currentPath)
  private[router] val RouteStates = HashMap.empty[String, Seq[OpenState[String]]]
  private val HashRegex = raw".*#(\S+)".r.anchored
  
  private val Origin =
    val locOrigin = window.location.origin.asInstanceOf[String|Null]
    if locOrigin != null && !locOrigin.isEmpty then locOrigin
    else window.location.protocol + "//" + window.location.host

  document.body.addEventListener("click", e => handleClick(e))
  window.addEventListener("popstate", _ => updatePathState())

  // todo is this one needed? Maybe call updatePathState() iff it has never been called before.
  //  document.addEventListener("DOMContentLoaded", () => updatePathState())

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
  
    val anchorOpt: Option[HTMLAnchorElement] = e.composedPath().find((evt: js.Dynamic) =>
      if evt.tagName.asInstanceOf[String|Unit] == "A" then true
      else false
    ).map(_.asInstanceOf[HTMLAnchorElement])
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

  private[router] def fullRegexString(root: String, path: Regex): String =
    var fixedRoot = root
    if !root.startsWith("/") then fixedRoot = "/" + fixedRoot
    if !fixedRoot.endsWith("/") then fixedRoot += "/"
    // ends with a non-capturing group to allow hashes
    "^" + fixedRoot + path.unanchored.regex.stripPrefix("/").stripSuffix("/") + "(?:/#.*)?"
  
  private[router] def buildParamStates(fullRegexString: String): Seq[OpenState[String]] =
    // count the # groups, attempting to ignore match groups.
    val numGroups = fullRegexString.sliding(2).count(w => w != raw"\(" && w.endsWith("("))
    val paramStates = Buffer.fill(numGroups)(useState(""))
    RouteStates.put(fullRegexString, paramStates)
    paramStates

  private def registerRoute(root: String, path: Regex, component: Component): Route =
    val frs = fullRegexString(root, path)
    RouteStates.get(frs)
      .map(paramStates => Route(frs.r, component, paramStates))
      .getOrElse(Route(frs.r, component, buildParamStates(frs)))

end Router

def useParams(routable: Routable, root: String = "/"): Seq[State[String]] =
  val frs: String = Router.fullRegexString(root, routable.path)
  Router.RouteStates.getOrElse(frs, Router.buildParamStates(frs))

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
