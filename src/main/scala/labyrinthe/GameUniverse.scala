package labyrinthe

import fr.istic.scribble.*
import Labyrinthe.*
import Cellule.*
import Jeu.*
import _root_.labyrinthe.Utils.labyrintheFourni

class GameUniverse(laby: Labyrinthe) extends Universe[Chemin] {

  val hauteur: Int = laby.hauteur
  val largeur: Int = laby.largeur
  val f: Agencement = laby.f

  override val HEIGHT: Int =
    taille_cellule.toInt * hauteur + 2 * epaisseur_mur.toInt + 5
  override val WIDTH: Int =
    taille_cellule.toInt * largeur + 2 * epaisseur_mur.toInt + 5
  override def init: Chemin = cheminInitial(laby)
  override val name: String = "Labyrinthe"

  override def react(chemin: Chemin, evenement: Event): Chemin =
    evenement match
      // Déplacements simples
      case KeyPressed(KeyUp)    => bouge(laby, chemin, Nord)
      case KeyPressed(KeyRight) => bouge(laby, chemin, Est)
      case KeyPressed(KeyDown)  => bouge(laby, chemin, Sud)
      case KeyPressed(KeyLeft)  => bouge(laby, chemin, Ouest)

      case KeyPressed(KeyAscii('#')) => init
      case KeyPressed(KeyAscii('a')) => annulerBouger(chemin)

      // Résolution par la strétégie de la main droite
      case KeyPressed(KeyAscii('n')) => bougeStrategieMainDroite(laby, chemin)
      case KeyPressed(KeyAscii('S')) =>
        resoudreParStrategieMainDroite(laby, chemin)
      case KeyPressed(KeyAscii('s')) => simplifie(chemin)
      case KeyPressed(KeyAscii('R')) => resoudre(laby, chemin)
      case KeyPressed(KeyAscii('i')) => indice(laby, chemin)

      // Résolution par force brute
      case KeyPressed(KeyAscii('E')) => etendreCheminJusquaArrivee(laby, chemin)
      case KeyPressed(KeyAscii('F')) => resoudreForceBrute(laby, chemin)
      case KeyPressed(KeyAscii('j')) => indiceForceBrute(laby, chemin)

      case _ => chemin

  override def stopWhen(c: Chemin): Boolean =
    false // Pour arrêter : utiliser ESC
  override def toImage(c: Chemin): Image =
    labToImage(laby, etatLabyrinthe(laby, c))

}