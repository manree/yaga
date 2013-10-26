/**
 *
 */
package client

/**
 * @author manuel
 * 
 * Client side data classes. 
 *
 */
class TrainingSession {}

abstract class Support


abstract class Exercise {
  def name: String
}

case class BarbellExercise(name: String) extends Exercise

case class DumbbellExercise(name: String) extends Exercise

case class BodyweightExercise(name: String) extends Exercise

abstract class ExerciseSet {
  
  def exercise: Exercise 
  def set: Int
  def rep: Int
  def support: Support
  def comment: String
}

case class BarbellSet(exercise: BarbellExercise, set: Int, rep: Int, weight: Double, support: Support, comment: String) extends ExerciseSet

case class DumbbellSet(exercise: DumbbellExercise, set: Int, rep: Int, weight: Double, support: Support, comment: String) extends ExerciseSet

case class BodyweightSet(exercise: BodyweightExercise, set: Int, rep: Int, support: Support, comment: String) extends ExerciseSet

case class NoSupport extends Support

case class Assisted(category: String, value: String) extends Support

case class Resisted(category: String, value: String) extends Support 
