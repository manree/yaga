package db
import scala.slick.driver.SQLiteDriver.simple._
import java.sql.Timestamp
import java.sql.Date

/**
 * @author manuel
 *
 * Server side data classes, corresponding to rows in database tables.
 */

case class WorkoutRow(date: String, id: Int, exercise: String, category: String, program: String, location: String) 

object WorkoutTable extends Table[WorkoutRow]("WORKOUT") {
	def id = column[Int]("ID", O.PrimaryKey)
	def date = column[String]("DATE")
	def exercise = column[String]("EXERCISE")
	def category = column[String]("CATEGORY")
	def program = column[String]("PROGRAM")
	def location = column[String]("LOCATION")
	def * = date ~ id ~ exercise ~ category ~ program ~ location <> (WorkoutRow, WorkoutRow.unapply _)
}

trait ExerciseRow {
	def timestamp: Timestamp 
	def workout_id: Int
	def name: String
	def comment: String
}

trait StrengthExerciseRow extends ExerciseRow {
  
  def set: Int
  def rep: Int
  
}

case class BarbellExerciseRow(timestamp: Timestamp, workout_id: Int, name: String, set: Int, rep: Int, weight: Double, comment: String) 
	extends StrengthExerciseRow

object BarbellTable extends Table[BarbellExerciseRow]("BARBELL") {
	def timestamp = column[Timestamp]("TIMESTAMP")
	//def date = column[String]("DATE")
	def workout_id = column[Int]("WORKOUT_ID")
	def name = column[String]("NAME")
	def set = column[Int]("SET")
	def rep = column[Int]("REP")
	def weight = column[Double]("WEIGHT")
	def comment = column[String]("COMMENT")
	def * = timestamp ~ workout_id ~ name ~ set ~ rep ~ weight ~ comment <> (BarbellExerciseRow, BarbellExerciseRow.unapply _)
	def pk = primaryKey("PK", (workout_id, name, set))
	def fk = foreignKey("FK", workout_id, WorkoutTable)(_.id)
}

case class DumbbellExerciseRow(timestamp: Timestamp, workout_id: Int, name: String, set: Int, 
		rep: Int, weight: Double, comment: String) extends StrengthExerciseRow

object DumbbellTable extends Table[DumbbellExerciseRow]("DUMBBELL") {
	def timestamp = column[Timestamp]("TIMESTAMP")
	def workout_id =  column[Int]("WORKOUT_ID")
	def name = column[String]("NAME")
	def set = column[Int]("SET")
	def rep = column[Int]("REP")
	def weight = column[Double]("WEIGHT")
	def comment = column[String]("COMMENT")
	def * = timestamp ~ workout_id ~ name ~ set ~ rep ~ weight ~ comment <> (DumbbellExerciseRow, DumbbellExerciseRow.unapply _)
	def pk = primaryKey("PK", (workout_id, name, set))
	def fk = foreignKey("FK", workout_id, WorkoutTable)(_.id)
}


case class SupportRow(id: Int, name: String, category: String, value: String)

object SupportTable extends Table[SupportRow]("SUPPORT") {
  def id = column[Int]("ID", O.PrimaryKey, O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def category = column[String]("CATEGORY", O.NotNull)
  def value = column[String]("VALUE")
  def * = id ~ name ~ category ~ value <> (SupportRow, SupportRow.unapply _)
  def idx = index("name-category", (name, category, value), unique = true)
}

case class BodyweightExerciseRow(timestamp: Timestamp, workout_id: Int, name: String, set: Int, rep: Int, supportId: Int, comment: String) 
	extends StrengthExerciseRow

object BodyweightTable extends Table[BodyweightExerciseRow]("BODYWEIGHT") {
	def timestamp = column[Timestamp]("TIMESTAMP")
	def workout_id =  column[Int]("WORKOUT_ID")
	def name = column[String]("NAME")
	def set = column[Int]("SET")
	def rep = column[Int]("REP")
	def supportId = column[Int]("SUPPORT_ID")
	def comment = column[String]("COMMENT")
	def * = timestamp ~ workout_id ~ name ~ set ~ rep ~ supportId ~ comment <> (BodyweightExerciseRow, BodyweightExerciseRow.unapply _)
	def pk = primaryKey("PK", (workout_id, name, set))
	def fk = foreignKey("FK", workout_id, WorkoutTable)(_.id)
}


