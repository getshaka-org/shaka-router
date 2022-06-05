package org.getshaka.shaka.router

import org.getshaka.shaka.*
import org.scalajs.dom.*

class Redirect(val path: String) extends Component:
  def template = Frag {
    apply()
  }

  def apply(): Unit =
    if path != window.location.pathname then window.location.assign(path)

object Redirect:
  def current = Redirect(window.location.pathname)
