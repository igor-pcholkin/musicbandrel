package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import javax.servlet.http.HttpServletRequest

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("org.random.musicbandrel")

    LiftRules.useXhtmlMimeType = false
    
    LiftRules.ajaxStart = Full(() => (LiftRules.jsArtifacts.show("ajax-loader").cmd & 
        LiftRules.jsArtifacts.hide("cytoscapeweb").cmd))
    LiftRules.ajaxEnd = Full(() => (LiftRules.jsArtifacts.hide("ajax-loader").cmd & 
        LiftRules.jsArtifacts.show("cytoscapeweb").cmd))	
}
  
}

