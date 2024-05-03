package labyrinthe
import Cellule.*
import fr.istic.scribble.*
import labyrinthe.Labyrinthe.EtatLabyrinthe

object Labyrinthe {

  val idsGroupes = List(
    "INFO-1",
    "INFO-1.2",
    "INFO-2",
    "INFO-3",
    "INFO-4 ",
    "INFO-5",
    "INFO-9",
    "MATHS-1",
    "MATHS-2"
  )

  val idsEnseignantTP = List(
    "adrien.leroch@univ-rennes.fr",
    "catherine.belleannee@irisa.fr",
    "dimitri.lajou@univ-rennes.fr",
    "gilles.lesventes@univ-rennes.fr",
    "killian.barrere@irisa.fr",
    "leo.cosseron@ens-rennes.fr",
    "pierre.lermusiaux@univ-rennes.fr",
    "raoul.vorch@irisa.fr",
    "thomas.anberree@univ-rennes.fr",
    "yasmine.hachani@inria.fr"
  )

  val authors = List(
    /* Identification de l'étudiant n°1 */
    Map(
      "nom" -> "Videcoq",
      "prénom" -> "Lucas",
      "numéro" -> "23111447",
      "email" -> "lucas.videcoq@etudiant.univ-rennes1.fr",
      "emailEnseignant" -> "leo.cosseron@ens-rennes.fr"
    ),

    /* Identification de l'étudiant n°2 */
    Map(
      "nom" -> "Merrer",
      "prénom" -> "Nicolas",
      "numéro" -> "22102431",
      "email" -> "nicolas.merrer@etudiant.univ-rennes1.fr",
      "emailEnseignant" -> "leo.cosseron@ens-rennes.fr"
    )
  )

  /** Positions des cellules dans un labyrinthe
    *
    * (0,0) est la position « au nord-ouest » d'un labyrinthe. (ou encore en
    *       haut à gauche).
    *
    * (i, j) est la position située à la ligne i, colonne j.
    *
    *                                       Nord
    *
    *                       (0,0) ----- j croissant  ------->
    *                         |  ┏━━━━━━━━━━━┳━┳━━━━━━━┳━━━━━┓
    *                         |  ┣━╸ ╺━━━━━┓ ┃ ╹ ╺━━━┓ ╹ ╻ ╻ ┃
    *              Ouest      i  ┃ ┏━━━┳━╸ ┗━┫ ╻ ╺━┳━┫ ╺━╋━┛ ┃   Est
    *                   croissant┃ ┃ ╻ ┣━╸ ┏━┫ ┃ ┏━┛ ╹ ╻ ┃ ╻ ┃
    *                         |  ┃ ╹ ┣━┛ ╻ ┃ ┃ ┃ ┃ ╻ ╻ ┣━┛ ┃ ┃
    *                         |  ┣━╸ ┣━┓ ┗━┛   ┗━┫ ┃ ┣━┛ ┏━┛ ┃
    *                         \/ ┗━━━━━┻━━━━━┻━━━┻━━━┻━━━┻━━━┛
    *
    *                                        Sud
    */
  type Position = (Int, Int)

  /** Agencement des murs d'un labyrinthe
    *
    * Un agencement est une fonction qui associe une cellule à chaque position,
    * indiquant ainsi les emplacements des murs.
    *
    * Un agencement est défini pour toutes les positions d'un plan infini et
    * décrit en théorie un labyrinthe infini dans les quatre directions.
    *
    * Cependant, pour un labyrinthe de dimensions données, peu importe
    * l'agencement des murs en dehors du labyrinthe. L'agencement ne sera
    * utilisé que pour l'intérieur du labyrinthe.
    */
  type Agencement = Position => Cellule

  /**  Agencement avec des murs partout
    *
    *  @note Utiliser une fonction anonyme.
    *        La solution tient en une courte ligne.
    */

  val agencementPlein: Agencement =
    Position =>
      Cellule(
        Ferme,
        Ferme
      ) // ( Position => for (x in range (Position._0)){for (y in range (Position._1)){Cellule(Ferme, Ferme)}})

  /** Agencement avec des murs nulle part
    */
  val agencementVide: Agencement = Position => Cellule(Ouvert, Ouvert)

  /** Type des labyrinthes
    *
    * @constructor crée un nouveau labyrinthe.
    *
    * @param hauteur le nombre de lignes du labyrinthe
    *
    * @param largeur le nombre de colonnes du labyrinthe
    *
    * @param f       l'agencement des murs intérieurs du labyrinthe
    *
    * @note - Par convention, la cellule (0,0) est « au nord-ouest. »
    *
    *       - L'agencement n'a besoin d'être correctement défini que
    *         sur les murs intérieurs du labyrinthe. En particulier,
    *         l'agencement peut ou non disposer des murs le long
    *         des bords du labyrinthe ; il n'en sera pas tenu compte
    *         pour afficher le labyrinthe ou lors du jeu (Les bords
    *         d'un labyrinthe sont toujours infranchissables).
    *         Vous n'avez donc pas à vous soucier de placer des murs
    *         sur pourtours d'un labyrinthe.
    */
  case class Labyrinthe(hauteur: Int, largeur: Int, f: Agencement)

  /** @param b un booléen
    * @return le passage Ouvert si b est vrai, Ferme sinon
    *
    * @note utile pour créer des agencements par manipulations de
    *       booléens.
    */
  def booleanToPassage(b: Boolean): Passage = {
    if b then Ouvert else Ferme
  } // TODO

  /** @param hauteur hauteur d'un labyrinthe
    * @param largeur largeur d'une labyrinthe
    * @return un agencement où tous les passages sont fermés sauf des couloirs
    *         au sud et à l'est.
    * @example couloirSudEst(4,5) est représenté par
    *
    *          ┏━┳━┳━┳━┳━┓
    *          ┣━╋━╋━╋━┫ ┃
    *          ┣━╋━╋━╋━┫ ┃
    *          ┣━┻━┻━┻━┛ ┃
    *          ┗━━━━━━━━━┛
    */
  def couloirSudEst(hauteur: Int, largeur: Int): Agencement = ??? // TODO

  /** @param nord un booléen
    * @param est un booléen
    * @return la cellule dont les passages Nord et Est sont spécifiés par les
    *         booléens donnés (true indiquant un passage ouvert)
    */
  def booleansToCellule(nord: Boolean, est: Boolean): Cellule = ??? // TODO

  /** @param hauteur nombre de lignes d'un labyrinthe
    * @param largeur nombre de colonnes d'un labyrinthe
    * @return un agencement en serpentin « horizontal », allant de l'entrée à la
    *         sortie du labyrinthe. La ligne la plus au sud doit permettre
    *         d'aller vers le nord uniquement depuis sa case la plus à l'est (en
    *         fin de ligne).
    *
    * @example serpentins de dimensions 4 x 5 et 5 x 5
    *
    *          ┏━━━━━━━━━┓           ┏━━━━━━━━━┓
    *          ┣━━━━━━━╸ ┃           ┃ ╺━━━━━━━┫
    *          ┃ ╺━━━━━━━┫           ┣━━━━━━━╸ ┃
    *          ┣━━━━━━━╸ ┃           ┃ ╺━━━━━━━┫
    *          ┗━━━━━━━━━┛           ┣━━━━━━━╸ ┃
    *                                ┗━━━━━━━━━┛
    */
  def serpentinH(hauteur: Int, largeur: Int): Agencement = ??? // TODO

  /** @param hauteur nombre de lignes d'un labyrinthe
    * @param largeur nombre de colonnes d'un labyrinthe
    * @param g une fonction fournissant un agencement dépendant de dimensions
    *          (tel que serpentinH et couloirSudEst)
    * @return le labyrinthe de dimensions hauteur x largeur et d'agencement
    *         g(hauteur, largeur)
    */
  def labyrinthe(
      hauteur: Int,
      largeur: Int,
      g: (Int, Int) => Agencement
  ): Labyrinthe = Labyrinthe(hauteur, largeur, g(hauteur, largeur))

  /* Affichage graphique d'un labyrinthe */

  /** @param laby un labyrinthe
    * @return la position de l'entrée du labyrinthe laby,
    *         qui par convention est dans le coin
    *         « sud-ouest. »
    */
  def entreeLabyrinthe(laby: Labyrinthe): Position =
    (laby.hauteur, 0)

    /** @param laby un labyrinthe
      * @return la position de la sortie du labyrinthe laby,
      *         qui par convention est « au nord-est. »
      */
  def sortieLabyrinthe(laby: Labyrinthe): Position =
    (0, 0)

  /** EtatLabyrinthe
    *
    * L'état d'un labyrinthe décrit les états de toutes ses cellules.
    *
    * Voir la documentation de EtatCellule pour plus d'information.
    */
  type EtatLabyrinthe = Position => EtatCellule

  /** @param laby un labyrinthe
    * @return l'état initial du labyrinthe laby, indiquant que
    *         la cellule d'entrée est la cellule courante
    *         et toutes les autres sont non visitées.
    */
  def etatInitial(laby: Labyrinthe): EtatLabyrinthe = EtatLabyrinthe(0, 0) =
    Courante(Nil) // TODO

  /** @param laby Un labyrinthe
    * @param etat Une fonction décrivant l'état de chaque cellule
    * @return Une image représentant le labyrinthe.
    *
    * @note utiliser la fonction celluleToImage et les images
    *       marqueurEntree et marqueurSortie définies dans Cellule.scala
    *
    *       Indication de longueur : une vingtaine de lignes au total,
    *       décomposées en fonctions ou expressions auxiliaires courtes.
    */
  def labToImage(laby: Labyrinthe, etat: EtatLabyrinthe): Image =

    /** @param p une position dans le labyrinthe laby
      * @return optionnellement, le marqueur à placer dans la cellule de position
      *         p du labyrinthe laby.
      */
    def marqueur(p: Position): Option[Image] = ??? // TODO

    /** @param p une position dans le labyrinthe laby
      * @return l'image de la cellule de laby à cette position
      */
    def imageCellule(p: Position): Image = ??? // TODO

    /** @param i un numéro de ligne du labyrinthe laby
      * @return l'image de cette ligne
      *
      * @note une solution courte est de la forme
      *       (0 until laby.largeur).foldRight ...
      *
      *       (0 until n) étant la liste des nombres de 0 à n-1.
      */
    def ligneToImage(i: Int): Image = ??? // TODO

    /** image du labyrinthe sans les enceintes extérieures */
    val raw: Image = ??? // TODO

    /* Ajout des enceintes entourant le labyrinthe
       Vous n'avez rien à compléter ci-dessous.
     */
    val enceinteH: Image = black(
      Rectangle(laby.largeur * taille_cellule, epaisseur_mur)
    )
    val enceinteV: Image = black(
      Rectangle(epaisseur_mur, laby.hauteur * taille_cellule)
    )

    Below(
      Beside(
        enceinteV,
        onAlign(Right, enceinteV, onAlign(Top, enceinteH, raw))
      ),
      Beside(jointure, enceinteH)
    )

}
