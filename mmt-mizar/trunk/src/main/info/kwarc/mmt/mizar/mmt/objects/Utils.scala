package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.translator.TranslationController
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._


object Mizar {
	val baseURI = URI("http", "oaff.mathweb.org") / "mml" 

	val MizarTh = DPath(mmt.baseURI / "foundations"/ "mizar") ? "Mizar-Curry"
	val MizarPatternsTh = DPath(mmt.baseURI / "foundations"/ "mizar") ? "mizar-patterns"
	val HiddenTh = DPath(mmt.baseURI / "foundations" / "mizar") ? "HIDDEN"
	//val TarskiTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/mizar_tarski.omdoc")) ? "Tarski"
	
	def constant(name : String) : Term = {
		name match {
			case "set" => OMID(HiddenTh ? name)
			case "=" => OMID(HiddenTh ? "==") //due to twelf constant naming limitations
			case "<>" => OMID(HiddenTh ? "<>")
			case _ => OMID(MizarTh ? name)
		}
		
	}
	
	def compact(t : Term) : Term = {
		t
	}
}

object MMTUtils {
	
	
	def getTheoryPath(aid : String) : MPath = 
	  if (aid == TranslationController.currentAid)
	    TranslationController.currentTheory
	  else aid match {
	  	case "HIDDEN" => Mizar.HiddenTh
	  	case _ =>  DPath(Mizar.baseURI) ? aid
	}
	
	def getPath(aid : String, kind : String, absnr : Int) : GlobalName = {
		getTheoryPath(aid) ? (aid + "_" + kind+ "_" + absnr.toString)
		
	}
	def getPath(aid : String, name : String) : GlobalName = {
		getTheoryPath(aid) ? name
	}
	
	def getPath(aid : String, name : List[String]) : GlobalName = {
	  getTheoryPath(aid) ? LocalName(name.map(NamedStep)) 
	}
}
