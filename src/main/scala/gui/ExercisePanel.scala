/**
 *
 */
package gui

import swing._
import event._
import swing.BorderPanel.Position._
import javax.swing.BorderFactory
import collection.mutable.Buffer
import collection.SortedMap
import collection.immutable.TreeMap
import client._
import db.Controller

case object SubmitEvent extends Event

case class SetField(set: Int) extends TextField {
  text = "" + set
  editable = false
  columns = 2
}

/**
 * @author manuel
 *
 * @param exercise the appropriate
 * @param controller the controller that handles the interaction
 * with the server-side data handling
 */
abstract class ExercisePanel
  extends BorderPanel {

  def exercise: Exercise

  def controller: Controller

  preferredSize = new Dimension(500, 500)
  border = BorderFactory.createLineBorder(java.awt.Color.BLACK)

  /**
   *  The abstract methods
   */
  def collect(): List[ExerciseSet]

  def submit(results: Iterable[ExerciseSet]): Unit = {

    // keep only those with nonzero reps
    val filtered_results = results filter (ex => ex.rep > 0)

    controller.insert(filtered_results, exercise)
  }

  def createSetRowPanel(set: Int): Panel

  /**
   * End of the abstract methods.
   */

  val submitButton = new Button { text = "Submit" }
  val addButton = new Button { text = "Add Set" }
  val exitButton = new Button { text = "Exit" }

  var set: Int = 0

  val setPanel = new BoxPanel(orientation = Orientation.Vertical) {
    border = BorderFactory.createLineBorder(java.awt.Color.BLACK)

    //contents += createSetRowPanel(set)
  }

  add(new Label(exercise.name), North)
  add(setPanel, Center)
  add(new FlowPanel {
    contents += exitButton
    contents += addButton
    contents += submitButton
  }, South)

  // unregister buttons (just to be sure) and publish Event
  def exit = {
    deafTo(addButton, submitButton, exitButton)
    publish(SubmitEvent)
  }

  listenTo(addButton, submitButton, exitButton)

  reactions += {

    case ButtonClicked(`addButton`) => {
      set += 1
      setPanel.contents += createSetRowPanel(set)
      // Call revalidate on a Panel that has its layout changed
      setPanel.revalidate
    }

    case ButtonClicked(`submitButton`) => {

      deafTo(addButton, submitButton, exitButton)

      submit(results = collect)

      Dialog.showMessage(this, message = "Saving " + exercise + " results", title = "Submitting...")

      /** tell the parent or main frame that this panel is done */
      publish(SubmitEvent)
    }

    case ButtonClicked(`exitButton`) => {

      if (set < 1) exit
      else {
        import Dialog._
        showConfirmation(this, message = "Would you like to exit without saving?",
          title = "Really exit?") match {

            case Result.Yes => exit
            case Result.No => println("Goin' back to Tuscon")
          }
      }
    }

    case ButtonClicked(b) => { println("What happenend? Who clicked " + b) }
  }

  abstract class SetRowPanel(set: Int) extends FlowPanel {

    border = BorderFactory.createLineBorder(java.awt.Color.BLACK)
    background = java.awt.Color.GRAY
  }
}

class BarbellExercisePanel(val exercise: BarbellExercise, val controller: Controller)
  extends ExercisePanel {

  var setMap: SortedMap[Int, (TextField, TextField)] = TreeMap()

  //

  def createSetRowPanel(set: Int): Panel = new BarbellSetRowPanel(set)

  def collect(): List[ExerciseSet] = {

    val noSupport = NoSupport

    var setC: Int = 0
    val result = for {
      (s, (r, w)) <- setMap
      rep = r.text.toInt
      weight = w.text.toDouble
      if (rep > 0 && weight > 0.0)
    } yield BarbellSet(
      exercise = exercise, set = s, rep = rep,
      weight = weight, support = noSupport, comment = "")

    result.toList
  }

  class BarbellSetRowPanel(set: Int) extends SetRowPanel(set) {

    val sf = new SetField(set)
    val rf = new TextField { text = "0"; columns = 3 }
    val wf = new TextField { text = "0.0"; columns = 5 }
    contents += new Label("Set:")
    contents += sf
    contents += new Label("Rep:")
    contents += rf
    contents += new Label("Weight:")
    contents += wf

    setMap += (set -> (rf, wf))
  }

}

class DumbbellExercisePanel(val exercise: DumbbellExercise, val controller: Controller)
  extends ExercisePanel {

  var setMap: SortedMap[Int, (TextField, TextField)] = TreeMap()

  //  def controller = db.JournalCreator

  def createSetRowPanel(set: Int): Panel = new DumbbellSetRowPanel(set)

  def collect(): List[DumbbellSet] = {

    val noSupport = NoSupport

    val result = for {
      (s, (r, w)) <- setMap
      rep = r.text.toInt
      weight = w.text.toDouble
      if (rep > 0 && weight > 0.0)
    } yield DumbbellSet(
      exercise = exercise,
      set = s, rep = rep, weight = weight, support = noSupport, comment = "")

    result.toList
  }

  class DumbbellSetRowPanel(set: Int) extends SetRowPanel(set) {

    val sf = new SetField(set)
    val rf = new TextField { text = "0"; columns = 3 }
    val wf = new TextField { text = "0.0"; columns = 5 }
    contents += new Label("Set:")
    contents += sf
    contents += new Label("Rep:")
    contents += rf
    contents += new Label("Weight:")
    contents += wf

    setMap += (set -> (rf, wf))
  }

}

class BodyweightExercisePanel(val exercise: BodyweightExercise, val controller: Controller)
  extends ExercisePanel {

  exercisePanel =>

  var setMap: SortedMap[Int, TextField] = TreeMap()

  var supportMap: SortedMap[Int, (ComboBox[String], ComboBox[String], TextField)] = TreeMap()

  // def controller = db.JournalCreator

  def createSetRowPanel(set: Int): Panel = new BodyweightSetRowPanel(set)

  def collect(): List[BodyweightSet] = {

    val result = for {

      (s, r) <- setMap
      (sup, suptpe, supval) <- supportMap get s
      support: Support = sup.selection.item match {
        case "Assisted" => Assisted(category = suptpe.selection.item, value = supval.text.toString)
        case "Resisted" => Resisted(category = suptpe.selection.item, value = supval.text.toString)
        case "No-support" => NoSupport
      }
      rep = r.text.toInt
      if (rep > 0)

    } yield BodyweightSet(
      exercise = exercise,
      set = s, rep = rep, support = support, comment = "")

    result.toList
  }

  class BodyweightSetRowPanel(set: Int) extends SetRowPanel(set) {

    val setf = new SetField(set)
    val repf = new TextField { text = "0"; columns = 3 }
    val supCheck = new CheckBox { text = "supported?" }
    listenTo(supCheck)
    reactions += {
      case ButtonClicked(`supCheck`) => {
        println("selected: " + supCheck.selected)
        if (supCheck.selected) addSupportRow else removeSupportRow
      }
    }

    val setRow = new FlowPanel(new Label("Set:"), setf, new Label("Rep:"), repf,
      new Label("support"), supCheck)

    contents += setRow

    val supf = new ComboBox(List("No-support", "Assisted", "Resisted"))
    val supTf = new ComboBox(List("", "Banded", "Weight-belt", "Human"))
    val supVf = new TextField { text = ""; columns = 6 }

    val supportRow = new FlowPanel(new Label("Sup:"), supf, new Label("Type"), supTf,
      new Label("Value"), supVf)

    def removeSupportRow = {
      contents.clear
      println("Contents contains: " + contents)
      contents += setRow
      // and we need to clear the fields in supportRow:
      supf.selection.index_=(0)
      supTf.selection.index_=(0)
      supVf.text = ""

      exercisePanel.revalidate

    }

    def addSupportRow = {
      contents += supportRow
      exercisePanel.revalidate
    }

    setMap += (set -> repf)
    supportMap += (set -> (supf, supTf, supVf))

  }

}
