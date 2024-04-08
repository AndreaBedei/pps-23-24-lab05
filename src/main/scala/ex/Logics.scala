
import util.Optionals.Optional
import util.Sequences.*
import scala.util.Random
trait Logics:
  def hit(x: Int, y: Int): Optional[Int]
  def won(): Boolean

object Logics:
  def apply(gridSize: Int, mines: Int): Logics = LogicsImpl(gridSize, mines)
  private class LogicsImpl(private val gridSize: Int, private val mines: Int) extends Logics:
    private var selected: Sequence[(Int, Int)] = Sequence.Nil()
    private var minesSet: Sequence[(Int, Int)] = Sequence.Nil()

    private val random = Random(42)
    private var inserted = 0
    while(inserted != mines)
      var x: (Int, Int) = (random.nextInt(gridSize), random.nextInt(gridSize))
      while (!minesSet.contains(x))
        x = (random.nextInt(gridSize), random.nextInt(gridSize))
      minesSet = Sequence.Cons(x, minesSet)
      inserted = inserted + 1
    println(minesSet)

    private def neighbours(x: Int, y: Int): Int =
      (x - 1 to x + 1).flatMap(xx => (y - 1 to y + 1)
        .map(yy => (xx, yy)))
        .count { case (xx, yy) => minesSet.contains((xx, yy)) }

    override def hit(x: Int, y: Int) =
      if minesSet.contains((x, y))
      then Optional.Empty()
      else
        selected = Sequence.Cons((x, y), selected)
        Optional.Just(neighbours(x, y))
    override def won() =
      def getSize(s: Sequence[(Int, Int)]): Int =
        var sum = 0
        s match
          case Sequence.Cons(x, y) =>
            sum = sum + 1
            getSize(y)
        sum

      getSize(selected) + getSize(minesSet) == gridSize * gridSize