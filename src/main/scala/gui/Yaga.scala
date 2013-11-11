/**
 *
 */
package gui

import swing._
import event._
import collection.immutable.TreeMap
import collection.SortedMap
import javax.swing.border.Border
import javax.swing.BorderFactory
import BorderPanel.Position._
import java.awt.event.FocusEvent
import client.BodyweightExercise
import client.BarbellExercise
import client.DumbbellExercise
import client.BodyweightExercise

/**
 * It is important to use call by name for panel, otherwise the initialization of
 * components might give infinite recursion.
 */
class GoToPanelButton(text: String, panel: => Panel, mainframe: => MainFrame) extends Button(text) {

  listenTo(this)
  reactions += {
    case ButtonClicked(_) => {
      mainframe.contents = panel
      mainframe.repaint
    }
  }
}



/**
 * @author manuel
 *
 */
object Yaga extends SimpleSwingApplication {

  val controller: db.Controller = db.Thingy

  def top = new MainFrame {
    // alias for the mainframe
    mainframe =>
    title = "Yaga"
    contents = StartPanel

    def open(panel: Panel): Unit = {
      mainframe.contents = panel
      mainframe.repaint
    }

    object StartPanel extends BorderPanel {
      preferredSize = new Dimension(500, 500)

      val exitBut = new Button("Workout done!") 
      
      val btnPnl = new BoxPanel(Orientation.Vertical) {
        contents += new GoToPanelButton("Select Exercise", ExerciseSelectPanel, mainframe)
        contents += new GoToPanelButton("Select History", HistoryPanel, mainframe)
        contents += exitBut
      }
      add(btnPnl, Center)
      listenTo(exitBut) 
      reactions += {
        case ButtonClicked(`exitBut`) => mainframe.close()
      }
    }

    object HistoryPanel extends BorderPanel {
      preferredSize = new Dimension(500, 500)
      val button = new GoToPanelButton("Go back to start", StartPanel, mainframe)
      add(new FlowPanel(button), East)
    }

    object ExerciseSelectPanel extends BorderPanel {

      val lst = controller.getExercises

      object exercisesList extends ListView(lst) {

        preferredSize = new Dimension(200, 200)
      }

      preferredSize = new Dimension(500, 500)
      val button = new GoToPanelButton("Go back to start", StartPanel, mainframe)
      add(new FlowPanel(button), North)
      add(exercisesList, Center)

      listenTo(exercisesList.mouse.clicks)
      
      reactions += {

        case MouseClicked(_, _, _, clicks, _) if clicks == 2 => {
          println("Go on" + " clicks " + clicks)
          if (exercisesList.selection.items.length == 1) {

            val exercises = exercisesList.selection.items(0)

            val panel = exercises match {
              case ex @ BodyweightExercise(n) => new BodyweightExercisePanel(ex, controller = controller)
             
              case ex @ BarbellExercise(n) => new BarbellExercisePanel(ex, controller = controller)
            
              case ex @ DumbbellExercise(n) => new DumbbellExercisePanel(ex, controller = controller)
            }

            mainframe.contents = panel
            mainframe.listenTo(panel)
            mainframe.reactions += {
              case SubmitEvent => {
                /* Unregister!!! */
                deafTo(panel)
                mainframe.contents = ExerciseSelectPanel
                mainframe.repaint
              }
            }
            mainframe.repaint

            println("You select to go to " +
              exercisesList.selection.items(0))
          }
        }
      }
    }
  }
}

