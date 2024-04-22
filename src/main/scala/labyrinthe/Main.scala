package labyrinthe


import fr.istic.scribble.*
import Labyrinthe.*
import Cellule.*
import Jeu.*
import Utils.*

object Main extends App {
  println("Projet Labyrinthe ... ")

  val laby: Labyrinthe = labyrintheFourni

  println(show(laby)) // Affiche le labyrinthe dans le terminal

  

  /* Décommentez la ligne suivante une fois que les fonctions labToImage et
     etatLabyrinthe sont définies : */

  // bigbang(new GameUniverse(laby))

  /* Si vous lancez cette application avec `sbt run` depuis un terminal,
     décommentez la ligne ci-dessous qui attend que l'utilisateur appuie sur la
     touche « entrée » afin que le jeu ne s'arrête pas immédiatement. */

  // val _ = scala.io.StdIn.readLine()

  

}