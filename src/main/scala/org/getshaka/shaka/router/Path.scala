package org.getshaka.shaka.router

import org.getshaka.shaka.Component

case class Path(pathParams: Seq[String], queryParams: Map[String, String])
