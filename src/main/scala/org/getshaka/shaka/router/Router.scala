package org.getshaka.shaka.router

import org.getshaka.shaka.{Binding, Component, ComponentBuilder, Element, OpenState, State, useState}

import scala.collection.mutable.{Buffer, HashMap}
import scala.collection.Seq
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex

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

  override val template: ComponentBuilder =
    Router.PathState.bind(newPath =>
      routes.find(_.regex.matches(newPath)) match
        case Some(Route(regex, component, paramStates)) =>
          component.render
          for (param, paramState) <- regex.findAllIn(newPath).zip(paramStates) do
              paramState.setValue(param)
          newPath match
            case Router.HashRegex(hashId) =>
              val elementToScroll = Router.Document.getElementById(hashId).asInstanceOf[Element|Null]
              if elementToScroll != null then elementToScroll.asInstanceOf[js.Dynamic].scrollIntoView()
              else Router.Window.scrollTo(0, 0)
            case _ =>
              Router.Window.scrollTo(0, 0)
        case None =>
          if catchAll != null then catchAll.render
    )

object Router:
  private val Window = js.Dynamic.global.window
  private val Document = js.Dynamic.global.document
  private val PathState = useState(currentPath)
  private[router] val RouteStates = HashMap.empty[String, Seq[OpenState[String]]]
  private val HashRegex = raw".*#(\S+)".r.anchored
  
  private val Origin =
    val locOrigin = Window.location.origin.asInstanceOf[String|Null]
    if locOrigin != null && !locOrigin.isEmpty then locOrigin
    else Window.location.protocol.asInstanceOf[String] + "//" + Window.location.host.asInstanceOf[String]

  Document.body.addEventListener("click", handleClick)
  Window.addEventListener("popstate", () => updatePathState())

  private def currentPath: String =
    var uri = Window.location.pathname.asInstanceOf[String].stripSuffix("/")
    val hash = Window.location.hash.asInstanceOf[String]
    if (!hash.isEmpty)
      uri += "/" + hash
    uri = js.Dynamic.global.decodeURI(uri).asInstanceOf[String]
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
    if (anchor.target != null && !anchor.target.isEmpty)
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
    if href != Window.location.href.asInstanceOf[String] then
      Window.history.pushState(js.Object(), "", href)
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

def useParams(root: String = "/", routable: Routable): Seq[State[String]] =
  val frs: String = Router.fullRegexString(root, routable.path)
  Router.RouteStates.getOrElse(frs, Router.buildParamStates(frs))

@js.native
@JSGlobal
private class HTMLElement extends js.Object:
  val tagName: String = js.native
  def hasAttribute(name: String): Boolean = js.native
  def getAttribute(name: String): String|Null = js.native

@js.native
@JSGlobal
private class HTMLAnchorElement extends HTMLElement:
  val target: String|Null = js.native
  val href: String|Null = js.native

@js.native
private trait MouseEvent extends js.Object:
  val defaultPrevented: Boolean = js.native
  val button: Int = js.native
  val metaKey: Boolean = js.native
  val ctrlKey: Boolean = js.native
  val shiftKey: Boolean = js.native
  def composedPath(): js.Array[js.Dynamic] = js.native
  def preventDefault(): Unit = js.native
