/**
 *
 */
package db

// Use SQLiteDriver to connect or create a sqlite database
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import java.sql.Date
import java.sql.Timestamp
import java.text.ParseException
import java.text.SimpleDateFormat
import scala.slick.lifted.DDL
import scala.language.implicitConversions
import scala.language.postfixOps
import com.sun.xml.internal.bind.v2.TODO
import client._

trait Controller {

  def currentWorkoutId: Int
  
  //def insert()

  /**
   *
   */
  def insert(sets: Iterable[ExerciseSet], exercise: Exercise): Unit = {

    println("Heidiho you submitted the following: " + sets + " for " + exercise)

  }

  def getExercises: Seq[Exercise] = Vector(
      BarbellExercise("Deadlift"), 
      BarbellExercise("Squat"), 
      BarbellExercise("Bench Press"), 
      BarbellExercise("Shoulder Press"), 
      BarbellExercise("Rows"), 
      BodyweightExercise("Push-ups"), 
      BodyweightExercise("Chin-ups"), 
      BodyweightExercise("Pull-ups"))

  //def insert[T >: Exercise](exercises: Iterable[T]): Unit
  //def update(oldExer: Exercise, newExer: Exercise): Unit

}

object PrintController extends Controller {
  val currentWorkoutId = 0
}

object Thingy extends Controller {

  val fileName = System.getProperty("user.dir") + "/journal.db"
  val dbFile = new java.io.File(fileName)
  lazy val db: Database = Database.forURL("jdbc:sqlite:" + fileName,
    driver = "org.sqlite.JDBC")

  def timestamp: Timestamp = new java.sql.Timestamp(System.currentTimeMillis())

  lazy val currentWorkoutId: Int = {
    
	 db withSession {
      
	  val q = Query(WorkoutTable)
      val ids: Seq[Int] = q.map(_.id).to[Seq]
      val maxId = if(ids.isEmpty) 0 else ids.max
    
      maxId + 1
    }
    
  }

  /**
   *  Inserts the support into the support table and returns the id of the support.
   *  An already existing support is not inserted but its id returned instead.
   */
  def insertSupport(support: Support): Int = {

    val (name, category, value) = support match {
      case Assisted(c, v) => ("Assisted", c, v)
      case Resisted(c, v) => ("Resisted", c, v)
      case NoSupport => ("No-support", "", "")
    }

    // Now check in db if support already exists. If true, retrieve its sup_id, else
    // get the next available sup_id and insert a new support row.
    db withSession {
      val q = Query(SupportTable)
      val ids: Seq[Int] = q.map(_.id).to[Seq]

      val maxId = if(ids.isEmpty) 0 else ids.max

      val idQuery = q.filter(r => r.name === name && r.category === category && r.value === value).map(_.id)

      val indexL: List[Int] = idQuery.to[List]

      val id = indexL match {
        case i :: Nil => i // nothing to be inserted
        case Nil => {
          val newId = maxId + 1
          SupportTable.insert(SupportRow(id = newId,
            name = name, category = category, value = value))
          newId
        }
        case _ => throw new IllegalStateException
      }

      // result of this function is the id of the row found or created:
      id
    }
  }

  def insertBarbellSets(sets: Iterable[ExerciseSet], exercise: String) {

	 val rows = for {
		 
      set <- sets
      
      // leave out the val keyword !
      row = set match {
        case BarbellSet(e, st, rp, w, support, c) => BarbellExerciseRow(
          timestamp = timestamp,
          workout_id = currentWorkoutId,
          name = e.name, set = st, rep = rp, weight = w, comment = c)
        case b => throw new IllegalStateException("Unexpected type in this set list: " + b)
      }
    } yield row
    

    // loan pattern: See Odersky et al chapter 9.
    // the session is loaned to the insert method.
    val session: Session = db.createSession

    try {
      
      WorkoutTable.insert(WorkoutRow(date=new Date(timestamp.getTime).toString(), 
          id=currentWorkoutId, exercise=exercise, category="Barbell", 
          program="", location=""))(session)
      rows.foreach(BarbellTable.insert(_)(session))
    
    } finally { session.close() }
  }

  def insertDumbbellSets(sets: Iterable[ExerciseSet], exercise: String) {

    val rows = for {

      set <- sets

      row = set match {
        case DumbbellSet(e, st, rp, w, support, c) => DumbbellExerciseRow(
          timestamp = timestamp,
          workout_id = currentWorkoutId,
          name = e.name, set = st, rep = rp, weight = w, comment = c)
        case b => throw new IllegalStateException("Unexpected type in this set list: " + b)
      }
    } yield row

    // loan pattern: See Odersky et al chapter 9.
    // the session is loaned to the insert method.
    val session: Session = db.createSession

    try {
        WorkoutTable.insert(WorkoutRow(date=new Date(timestamp.getTime).toString(), 
          id=currentWorkoutId, exercise=exercise, category="Dumbell", 
          program="", location=""))(session)
      rows.foreach(DumbbellTable.insert(_)(session))
    } finally { session close }
  }

  def insertBodyweightSets(sets: Iterable[ExerciseSet], exercise: String) {

    val rows = for {

      set <- sets

      row = set match {
        case BodyweightSet(e, st, rp, support, c) => BodyweightExerciseRow(
          timestamp = timestamp,
          workout_id = currentWorkoutId,
          name = e.name, set = st, rep = rp, supportId = insertSupport(support), comment = c)

        case b => throw new IllegalStateException("Unexpected type in this set list: " + b)
      }
    } yield row

    // loan pattern: See Odersky et al chapter 9.
    // the session is loaned to the insert method.
    val session: Session = db.createSession

    try {
      WorkoutTable.insert(WorkoutRow(date=new Date(timestamp.getTime).toString(), 
          id=currentWorkoutId, exercise=exercise, category="Bodyweight", 
          program="", location=""))(session)
      rows.foreach(BodyweightTable.insert(_)(session))
    } finally { session close }
  }

  //support match {
  //		-case Assisted(t, v) => Some(SupportRow(getNextSupId, name="Assisted", category=t, value=v))
  //		case Resisted(t, v) => Some(SupportRow(getNextSupId, name="Resisted", category=t, value=v))
  //		case NoSupport() => None
  //}}

  override def insert(sets: Iterable[ExerciseSet], exercise: Exercise) {

    exercise match {
      case BarbellExercise(_) => insertBarbellSets(sets, exercise.name)
      case DumbbellExercise(_) => insertDumbbellSets(sets, exercise.name)
      case BodyweightExercise(_) => insertBodyweightSets(sets, exercise.name)
      case _ => throw new UnsupportedOperationException()
    }
  }

}

/**
 * @author manuel
 *
 */
object JournalCreator extends App {

  // converter for strings to java.sql.Date
  implicit def stringToDate(date: String): Date = new Date(
    new SimpleDateFormat("yyyymmdd").parse(date).getTime())

  val fileName = System.getProperty("user.dir") + "/journal.db"
  val dbFile = new java.io.File(fileName)
  val existed = dbFile.exists()

  def timestamp: Timestamp = new java.sql.Timestamp(System.currentTimeMillis())

  def getNextSupportId = 1

  Database.forURL("jdbc:sqlite:" + fileName,
    driver = "org.sqlite.JDBC") withSession {

      if (!existed) {
        println("Creating table")
        (BarbellTable.ddl ++ DumbbellTable.ddl ++ BodyweightTable.ddl
          ++ SupportTable.ddl ++ WorkoutTable.ddl).create
      }

      val ts1 = timestamp
      val id1 = 8
      WorkoutTable.insert(WorkoutRow(new Date(ts1.getTime()).toString(), id1, "Squat", "Barbell", "At home", "SL5x5"))
      BarbellTable.insert(BarbellExerciseRow(ts1, id1, "Squat", 1, 5, 52.5, ""))
      BarbellTable.insert(BarbellExerciseRow(ts1, id1, "Squat", 2, 5, 52.5, "Pfff"))

      val ts2 = timestamp
      val id2 = 9
      WorkoutTable.insert(WorkoutRow(new Date(ts2.getTime()).toString(), id2, "Squat", "Barbell", "At home", "SL5x5"))
      BarbellTable.insert(BarbellExerciseRow(ts2, id2, "Squat", 1, 5, 55.0, "Tjongejonge"))
      BarbellTable.insert(BarbellExerciseRow(ts2, id2, "Squat", 2, 5, 55.0, "Pfff"))

      val q1 = for {
        rec <- WorkoutTable leftJoin BarbellTable on (_.id === _.workout_id)
      } yield rec

      q1 foreach println

      val q2 = for {
        (w, b) <- WorkoutTable leftJoin BarbellTable on (_.id === _.workout_id)
      } yield (w.date, w.id, w.exercise, b.set, b.rep, b.weight)

      q2 foreach println

      //Thread.sleep(10)
      //BarbellTable.insert(BarbellExercise(ts1+2, "Squat", 1, 5, 52.5, ""))
      //BarbellTable.insert(BarbellExercise(ts1+3, "Squat", 1, 5, 52.5, ""))
      //BarbellTable.insert(BarbellExercise(ts1+4, "Squat", 1, 5, 52.5, ""))
      //BarbellTable.insert(BarbellExercise(ts1, "Squat", 1, 5, 52.5, ""))

      //     val ex = Exercise("20131006", "Squat", 1, 5, 52.5)

      //println(Strength.toString)

      def show = { Query(BarbellTable) foreach println; println("blablabla") }

      //Strength.insert(Exercise(timestamp, "DeadLift", 3, 5, 105.0))

      Query(BarbellTable).where(_.name is "Squat") foreach println

      show

      val exset = Set("Squat", "Press")

      //  Strength.insert(Exercise(new Date(1000L * 100000L), "Squat", 2, 5, 55))

      val upd1 = for { s <- BarbellTable if (s.name === "Squal") } yield s.weight
      upd1.update(60.0)

      // How to do a set operation?
      // 

      // Where the hell does the method 'inSet' come from?
      // Found it: in ColumnExtensionMethods
      // Ok good, now I know how to do set operations.
      val upd2 = for {
        s <- BarbellTable
        if (s.name inSet Set("Squat", "Bench"))
      } yield s.rep
      upd2.update(10)

      BarbellTable.map(_.weight) foreach println
      println("yodedo")

      val exercise = for {
        s <- BarbellTable

      } yield s.name

      val l = exercise.list
      val e = BarbellTable.map(_.rep).to[Vector]

      println("head = " + l.head + ", " + e)

      //val m: Double = w.max
      //println(w +"\t"+ w.max + "\t" + w.min)
    }
}


