package labyrinthe

import Labyrinthe.*
import Cellule.*

object Jeu {

  /** Type représentant le chemin parcouru par le joueur lors d'une déambulation
    * dans un labyrinthe
    *
    * Un chemin n'est jamais vide, car il contient au moins la position courante
    * du joueur, qui est toujours à l'intérieur du labyrinthe.
    *
    * Le chemin est sauvegardé dans l'ordre inverse du parcours :
    *
    *   - premier élément de la liste : position courante
    *
    *   - dernier élément de la liste : position de départ
    *
    * Nous ne considérerons que des chemins valides. Un chemin est valide si
    *
    *   - il n'est pas vide
    *   - toute position du reste du chemin est voisine de la position précédente
    */
  type Chemin = List[Position]

  /** @param chemin un chemin de labyrinthe
    * @return la position du joueur dans le labyrinthe selon le chemin donné
    */
  def positionCourante(chemin: Chemin): Position = {
    chemin match
      case x :: _ => x
  }

  /** @param p une position dans un labyrinthe
    * @param chemin un chemin d'un labyrinthe
    * @return true ssi la position p est la position courante du joueur selon le
    *         chemin donné.
    */
  def estPositionCourante(p: Position, chemin: Chemin): Boolean =
    p == positionCourante(chemin)

  /** @param laby un labyrinthe
    * @param chemin un chemin dans laby
    * @return vrai ssi le chemin mène à la sortie de laby.
    */
  def estResolu(laby: Labyrinthe, chemin: Chemin): Boolean = {
    chemin match
      case x :: _ => x == sortieLabyrinthe(laby)
  }

  /** @param laby un labyrinthe
    * @return le chemin initial, c'est-à-dire celui avec lequel on commence à jouer.
    */
  def cheminInitial(laby: Labyrinthe): Chemin = entreeLabyrinthe(laby) :: Nil

  /** @param chemin un chemin
    * @return le chemin correspondant à l'annulation du dernier déplacement dans chemin,
    *         si des déplacements ont déjà été effectués.
    */
  def annulerBouger(chemin: Chemin): Chemin = {
    chemin match
      case x :: Nil => chemin
      case x :: y   => y
  }

  /** Directions des déplacements dans un labyrinthe */
  sealed trait Direction
  case object Nord extends Direction
  case object Sud extends Direction
  case object Est extends Direction
  case object Ouest extends Direction

  /** Liste des directions */
  val directions: List[Direction] = Ouest :: Sud :: Est :: Nord :: Nil

  /** @param p une position
    * @param d une direction
    * @return la position voisine de pos, suivant direction
    */
  def voisine(p: Position, d: Direction): Position = (d, p) match
    case (Nord, (x, y))  => (x - 1, y)
    case (Sud, (x, y))   => (x + 1, y)
    case (Est, (x, y))   => (x, y + 1)
    case (Ouest, (x, y)) => (x, y - 1)

  /** @param laby un labyrinthe
    * @param p une position
    * @param d une direction
    * @return vrai ssi p est à l'intérieur du labyrinthe laby et il existe un passage
    *         depuis la position p selon la direction d permettant de rester à
    *         l'intérieur du labyrinthe.
    *
    * @note   Souvenez-vous qu'un agencement de labyrinthe ne place pas
    *         nécessairement de murs sur le pourtour du labyrinthe. Ces murs
    *         seront dessinés quoi qu'il en soit. Le passage de l'intérieur à
    *         l'extérieur du labyrinthe doit donc être interdit, quel que soit
    *         l'agencement.
    *
    *         Indication de longueur : 10 à 20 lignes
    */
  def passageOuvert(
      laby: Labyrinthe,
      p: Position,
      d: Direction
  ): Boolean = {
    d match {
      case Nord => laby.f(p) match { case Cellule(e, _) => e == Ouvert }
      case Est  => laby.f(p) match { case Cellule(_, e) => e == Ouvert }
      case Sud =>
        laby.f(voisine(p, d)) match { case Cellule(e, _) => e == Ouvert }
      case Ouest =>
        laby.f(voisine(p, d)) match { case Cellule(_, e) => e == Ouvert }
    }
  }

  /** @param laby un labyrinthe
    * @param p une position dans laby
    * @return une liste de positions voisines accessibles depuis p dans laby
    *
    * @note utiliser directions, filter et map
    */
  def voisines(laby: Labyrinthe, p: Position): List[Position] =
    directions.filter(y => passageOuvert(laby, p, y)).map(y => voisine(p, y))

  /** @param laby un labyrinthe
    * @param chemin un chemin dans le labyrinthe laby
    * @param direction une direction
    * @return l'extension du chemin d'une case dans la direction donnée,
    *         si possible, ou sinon le chemin donné
    */
  def bouge(
      laby: Labyrinthe,
      chemin: Chemin,
      direction: Direction
  ): Chemin = if passageOuvert(laby, chemin.head, direction)
  then (voisine(chemin.head, direction) :: chemin)
  else (chemin)

  /** @param p0 une position dans un labyrinthe
    * @param p1 une position dans un labyrinthe
    * @return la direction de déplacement de p0 vers p1
    *
    * @note indication de longueur : moins de 10 lignes
    */
  def directionDeplacement(p0: Position, p1: Position): Direction =
    (p0._1 - p1._1, p0._2 - p1._2) match
      case (-1, 0) => Nord
      case (1, 0)  => Sud
      case (0, -1) => Ouest
      case (0, 1)  => Est

  /** @param chemin un chemin
    * @return la direction dans laquelle se déplace le joueur ayant suivi ce chemin,
    *         déterminée pas les deux dernières positions. Par convention, nous
    *         stipulons que le sens de déplacement est l'Est lorsque le chemin
    *         est initial.
    *
    * @note indication de longueur : moins de 5 lignes
    * @note utiliser la fonction directionDeplacement
    */
  def sensDeplacement(chemin: Chemin): Direction = chemin match
    case x :: y :: _ => directionDeplacement(x, y)
    case _           => Est

  /** @param laby un labyrinthe
    * @param chemin un chemin dans le labyrinthe laby
    * @return une fonction décrivant l'état de chaque cellule du labyrinthe laby
    *
    * @note PHASE 1 : lorsqu'une cellule a été visitée, contentez-vous de
    *       définir son état comme `Visitee(Nil)`. Dans ce cas, l'état de la
    *       cellule ne contiendra pas assez d'information pour savoir comment
    *       dessiner précisément la trace du passage du joueur dans cette
    *       cellule et cette trace sera uniquement indiquée par un point bleu
    *       (cf. fonction `trace` dans le fichier `Cellule.scala`)
    *
    *       indication de longueur : 5 lignes environ (Phase 1)
    *
    * @note PHASE 2 : à ne tenter que si tout le reste est fait et qu'il vous
    *       reste beaucoup de temps. Lisez la spécification du type EtatCellule
    *       qui concerne la phase 2 dans le fichier Cellule.scala.
    *
    *       N'hésitez pas à demander de l'aide à votre encadrant de TP.
    *
    *       Indication de longueur pour la phase 2: 20 à 40 lignes,
    *                                               fonctions auxiliaires comprises.
    *
    * @note cf. Cellule.scala, definition de EtatCellule
    */
  def etatLabyrinthe(
      laby: Labyrinthe,
      chemin: Chemin
  ): EtatLabyrinthe = p =>
    if p == positionCourante(chemin) then Courante(Nil)
    else if chemin.contains(p) then Visitee(Nil)
    else NonVisitee

  /* ======================================*/
  /* RÉSOLUTION : recherche d'une solution */
  /* ======================================*/

  /* RÉSOLUTION PAR LA MÉTHODE DE LA MAIN DROITE
     ===========================================

     Un labyrinthe est parfait s'il existe un UNIQUE chemin entre deux cases
     quelconques. En particulier, un labyrinthe parfait ne comporte pas de
     boucles.

     Exemples de labyrinthes parfaits :

           ┏━━━━━┓     ┏━┳━━━┓     ┏━━━┳━┓
           ┃ ┏━┳━┫     ┃ ┃ ╻ ┃     ┃ ╺━┛ ┃
           ┃ ╹ ╹ ┃     ┃ ┗━┛ ┃     ┗━━━━━┛
           ┗━━━━━┛     ┗━━━━━┛

     Dans un labyrinthe parfait, la stratégie qui consiste à se déplacer en
     maintenant toujours sa main droite en contact avec le mur à notre droite
     nous assure de passer par toutes les cases du labyrinthe, en particulier sa
     sortie.

   */

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin valide dans le labyrinthe laby
    * @return un nouveau chemin représentant le chemin donné, allongé d'une case
    *         supplémentaire selon la stratégie de la main droite.
    *
    * @note indication de longueur : 10 à 20 lignes
    */
  def bougeStrategieMainDroite(
      laby: Labyrinthe,
      chemin: Chemin
  ): Chemin = ??? // TODO

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin dans le labyrinthe laby
    * @return le chemin prolongeant le chemin donné et menant à l'arrivée du
    *         labyrinthe laby en poursuivant les déplacements du joueur selon la
    *         stratégie de la maint droite
    *
    * @note indication de longueur : environ 5 lignes, y compris une fonction auxiliaire
    */
  def resoudreParStrategieMainDroite(
      laby: Labyrinthe,
      chemin: Chemin
  ): Chemin = ??? // TODO

  /** @param chemin un chemin d'un labyrinthe
    * @return le chemin obetenu en enlevant les détours éventuels du chemin
    *         donné.
    *
    * @note   Autrement dit, le chemin obtenu ne doit contenir que des positions
    *         déjà présente dans le chemin donné et ne doit pas passer deux fois
    *         par la même position.
    *
    * @note   indication de longueur : moins de 10 lignes.
    *
    * @note   Notre solution utilise la méthode span
    *         Cf. https://www.scala-lang.org/api/2.13.3/scala/collection/immutable/List.html#span(p:A=%3EBoolean):(List[A],List[A])
    */
  def simplifie(chemin: Chemin): Chemin = ??? // TODO

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin dans le labyrinthe laby
    * @return un chemin ÉTENDANT le chemin donné et menant à la sortie de laby
    *         de la façon la plus directe possible.
    *
    * @note l'ancien chemin doit rester un suffixe du nouveau chemin,
    *       c'est-à-dire que le chemin déjà parcouru par le joueur au moment où
    *       il demande la solution ne doit pas être modifié.
    *
    * @note indication de longueur : 1 À 3  lignes
    *
    * @note utiliser les fonctions simplifie et resoudreParStrategieMainDroite
    */
  def resoudre(laby: Labyrinthe, chemin: Chemin): Chemin = ??? // TODO

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin valide dans le labyrinthe laby
    * @return le chemin étendant le chemin donné d'une seule case le long du
    *         chemin direct vers la sortie à partir de la position du joueur.
    *
    * @note Indication de longueur : 1 à 3  lignes
    *
    *       La fonction takeRight : List[T] => Int => List[T]
    *       peut être utile. xs.takeRight(n) renvoie les n derniers
    *       éléments de la liste xs.
    */
  def indice(laby: Labyrinthe, chemin: Chemin): Chemin = ??? // TODO

  /* RÉSOLUTION PAR FORCE BRUTE (Très difficile)
     ===========================================

     Cette méthode consiste à explorer tous les chemins possibles à partir d'une
     poqirion donnée et à retenir celui qui mène à la sortie.

     Remarque : la stratégie précédente (main droite) était différente : elle
                considérait un seul chemin passant par toutes les positions du
                labyrinthe.

   */

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin quelconque dans le labyrinthe laby
    * @return une extension du chemin donné jusqu'à la sortie du layrinthe. Le
    *         prolongement du chemin doit être sans cycles (il ne doit pas
    *         repasser par l'une de ses propres cases) et sa première case ne
    *         doit pas être une case du chemin reçu en argument. Si cela n'est
    *         pas possible, renvoyer le chemin reçu.
    *
    * @note Utiliser voisines, filter, map et d'autres fonctions déjà définies
    *       pour exprimer l'idée suivante :
    *
    *       Une extension de chemin jusqu'à l'arrivée est obtenue en étendant le
    *       chemin d'une position parmi les voisines accessibles depuis la tête du
    *       chemin et n'appartenant pas déjà au chemin, puis en appliquant
    *       récursivement la fonction etendreCheminJusquaArrivee aux chemins étendus
    *       d'une case ainsi obtenus.
    *
    * @note indication de longueur : moins de 10 lignes
    */
  def etendreCheminJusquaArrivee(
      laby: Labyrinthe,
      chemin: Chemin
  ): Chemin = ??? // TODO

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin dans le labyrinthe laby
    * @return le chemin ÉTENDANT le chemin donné et menant à la sortie de laby
    *         de la façon la plus directe possible.
    *
    * @note l'ancien chemin doit rester un suffixe du nouveau chemin,
    *       c'est-à-dire que le chemin déjà parcouru par le joueur au moment où
    *       il demande la solution ne doit pas être modifié.
    *
    * @note Indication de longueur : 1 À 3  lignes
    *
    * @note utiliser la fonction etendreCheminJusquaArrivee
    */
  def resoudreForceBrute(
      laby: Labyrinthe,
      chemin: Chemin
  ): Chemin = ??? // TODO

  /** @param laby un labyrinthe parfait
    * @param chemin un chemin valide dans le labyrinthe laby
    * @return le chemin étendant le chemin donné d'une seule case le long du
    *         chemin direct vers la sortie à partir de la position du joueur.
    *
    * @note Indication de longueur : 1 à 3  lignes
    *
    * @note Utiliser la fonction resoudreForceBrute
    */
  def indiceForceBrute(laby: Labyrinthe, chemin: Chemin): Chemin = ??? // TODO

}
