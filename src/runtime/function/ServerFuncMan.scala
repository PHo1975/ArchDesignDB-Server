/**
 * Author: Peter Started:07.08.2010
 */
package runtime.function

import definition.expression.CommonFuncMan
/**
 * 
 */

// will be instanciated after initialisation of Session Manager



object ServerFuncMan extends CommonFuncMan {
  override val onlyIntUnits=Array("Stück","Stck","Pausch","Sack","Paar")

}
