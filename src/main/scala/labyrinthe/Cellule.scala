package labyrinthe
import fr.istic.scribble.*
import labyrinthe.Jeu.{Direction, Est, Nord, Ouest, Sud}

object Cellule {

  /** Passages
    *
    * Un passage ouvert indique une absence de mur.
    *
    * Un passage fermé indique la présence d'un mur.
    */
  sealed trait Passage
  case object Ferme extends Passage
  case object Ouvert extends Passage

  /** Cellule d'un labyrinthe
    *
    *  Chaque case d'un labyrinthe est décrite par une cellule. Une cellule
    *  indique la présence ou non de murs « au nord » ou « à l'est » de la case.
    *
    *  Il y a donc quatre dispositions de cellule possibles :
    *
    *      - Cellule(Ferme, Ferme)
    *      - Cellule(Ferme, Ouvert)
    *      - Cellule(Ouvert, Ferme)
    *      - Cellule(Ouvert, Ouvert)
    */
  case class Cellule(nord: Passage, est: Passage)

  /** EtatCellule : information sur le passage du joueur dans une cellule.
    *
    *     Durant le déroulement du jeu consistant à se déplacer dans le
    *     labyrinthe, on associe à chaque cellule un état indiquant si cette
    *     cellule est la cellule courante (là où le joueur se trouve) ou si elle
    *     a déjà a été visitée ou non par le joueur.
    */

  sealed trait EtatCellule
  case object NonVisitee extends EtatCellule
  case class Courante(ds: List[Direction]) extends EtatCellule
  case class Visitee(ds: List[Direction]) extends EtatCellule

  /* Explication de EtatCellule
   *
   *
   *     PHASE 1 :
   *
   *     Dans la première phase de ce projet, le paramètre ds sera simplement
   *     égal à Nil et donc inutile, puisqu'il ne représentera pas d'information
   *     supplémentaire. Le trajet du joueur sera simplement représenté par une
   *     succession de points bleus placés dans les cellules visitées et la
   *     cellule courante.
   *
   *     PHASE 2 : (À IGNORER tant que la phase 1 n'est pas terminée.)
   *
   *     Dans la seconde phase de ce projet, le paramètre ds est une liste de
   *     directions indiquant par où le joueur est entrée et ressortie de la
   *     cellule lors de ses différents passages éventuels dans la cellule.
   *
   *     Cette information supplémentaire permettra de tracer plus précisément
   *     les trajets du joueur dans le labyrinthe, en utilisant des traits
   *     continus plutôt que des points.
   *
   *     Par exemple :
   *
   *      - Au départ du jeu, la cellule de départ à pour état Courante(Nil) et
   *        toutes les autres cellules ont pour état NonVisitee
   *
   *      - Si le joueur se déplace d'une case vers l'Est, les états des
   *        cellules sont :

   *        - Visitee(Est::Nil)       pour la cellule de départ
   *        - Courante(Ouest::Nil)    pour la cellule à sa droite
   *        - NonVisitee              pour toutes les autres
   *
   *      - Si le joueur se déplace de nouveau d'une case vers l'Est, nous
   *        aurons trois cellules adjacentes avec les états suivant :
   *
   *        - Visitee(Est::Nil)                 pour la cellule de départ
   *        - Visitee(Ouest::Est::Nil)          pour la cellule à sa droite
   *        - Courante(Ouest::Nil)              pour la suivante à droite
   *
   *        et toutes les autres cellules ont pour état NonVisitee
   *
   *      - NonVisitee indique que le joueur n'est jamais passé dans la cellule.
   *
   *      - Visitee(Nil) et Courant(Nil) sont des états impossibles.
   *
   *      - Courante(Sud::Nil) indique que le joueur vient d'arriver depuis le
   *        sud dans la case et que c'est la première fois qu'il la visite.
   *
   *      - Visitee(Nord::Nil) indique que le joueur vient de quitter la cellule
   *        par le Nord et qu'il n'y était jamais entré. C'était donc la cellule
   *        de départ, mais il n'y est plus.
   *
   *      - Courante(Nord::Ouest::Sud::Nil) indique que joueur vient d'arriver
   *        depuis le Nord dans le cellule et qu'il l'avait déjà traversée en
   *        entrant par le Sud et en y ressortant pas l'Ouest.
   *
   *      - Visitee(Nord::Ouest::Nil) indique que le joueur est passé une fois
   *        par cette cellule, qu'il y est entré par l'ouest et resorti par le
   *        nord.
   *
   *      - Visitee(Est::Ouest::Sud:Est::Nil) signifie que le joueur est passé
   *        deux fois par la cellule, une première fois entrant depuis l'Est et
   *        en ressortant par le Sud, une seconde fois en entrant par l'Ouest et
   *        en ressortant par l'Est.
   *
   *      Ces informations permettent entre autre de tracer le trajet du joueur
   *      à l'intérieur de chaque cellule : il suffit de tracer des segements
   *      partant du centre de la cellule à chacun des bords indiqués par les
   *      directions de ds. Par exemple, en reprenant l'exemple de
   *      Visitee(Est::Ouest::Sud:Est::Nil), le tracer du trajet dans la cellule
   *      concernée formera un T.
   *
   */

  /* Fonctions auxiliaires sur type Image */

  /** @param w une longueur
    * @return un carré de côté de longueur w
    */
  def square(w: Float): Image = Rectangle(w, w)

  /** @param c une couleur
    * @param img une image
    * @return l'image img entièrement coloriée de la couleur c
    */
  def colorie(c: Color, img: Image): Image = LineColor(FillColor(img, c), c)

  /* Fonctions de coloriage avec une couleur spécifique */
  def red(img: Image): Image = colorie(RED, img)
  def black(img: Image): Image = colorie(BLACK, img)
  def blue(img: Image): Image = colorie(BLUE, img)
  def orange(img: Image): Image = colorie(Color(255, 128, 0, 255), img)
  def transparent(img: Image): Image = colorie(TRANSPARENT, img)

  /* Représentation graphique d'une cellule

     Cela vous sera utile pour définir le fonction labToImage dans
     labyrinthe.scala

   */

  val zoom: Float =
    2.5 // Facteur de zoom pour faciliter le redimensionnement d'un labyrinthe.
  val taille_cellule: Float = zoom * 12.0f // taille du côté d'une cellule
  val epaisseur_mur: Float = zoom * 1.0f // épaisseur des murs

  val mur_vertical: Image = black(Rectangle(epaisseur_mur, taille_cellule))
  val mur_horizontal: Image = black(Rectangle(taille_cellule, epaisseur_mur))

  /** Pour compléter le sommet de l'angle droit formé par certains murs.
    *
    * @note Vous pouvez remplacer `black` par `red` pour voir où les jointures sont utilisées,
    *       lorsque vous aurez dessiné votre premier labyrinthe
    */
  val jointure: Image = black(square(epaisseur_mur))

  /** Image transparente aux dimensions d'une cellule */
  val fond: Image = transparent(square(taille_cellule))

  /** Pour indiquer que la cellule a été visitée */
  val empreinte: Image = blue(Circle(taille_cellule / 3 - 3))

  /** Représentation du joueur dans la cellule courante */
  def joueur(ds: List[Direction]): Image =

    def degree(d: Direction): Float =
      d match
        case Est   => 0
        case Nord  => 90
        case Ouest => 180
        case Sud   => 270

    ds match
      case Nil => Rotate(orange(square(taille_cellule / 2)), 45)
      case d :: _ =>
        Rotate(
          orange(Triangle(taille_cellule - epaisseur_mur - 2)),
          210 + degree(d)
        )

  /*
     Nous appelons **zone intérieure** d'une cellule le carré de taille
     (taille_cellule - epaisseur_mur) aligné avec l'angle inférieur gauche de la
     cellule. C'est la partie non recouverte pas les murs nord et est.
   */

  /** @param motif une image dont les dimensions sont inférieures aux dimensions
    *              intérieures d'une cellule, tenant donc dans un carré de côté
    *              taille_cellule - epaisseur_mur.
    *
    * @return une image aux dimensions d'une cellule avec le motif donné placé
    *         au centre de la zone intérieure de la cellule.
    */
  def calqueCellule(motif: Image): Image =
    onBackAt(
      On(motif, transparent(square(taille_cellule - epaisseur_mur))),
      fond,
      0,
      -epaisseur_mur
    )

  /** @param direction une direction cardinale
    * @return une image de la trace du déplacement du joueur depuis le centre de
    *         la zone intérieure d'une cellule jusqu'au bord de la cellule dans
    *         la direction donnée
    */
  def traceVers(direction: Direction): Image =

    val ep: Float = taille_cellule / 3 - 2
    val lg: Float = taille_cellule / 2
    val origine: Image = blue(Circle(ep / 2))
    val layer: Image = calqueCellule(origine)
    val offset0: Float = -lg + ep / 2
    val offsetX: Float = offset0 + epaisseur_mur / 2
    val offsetY: Float = offset0 - epaisseur_mur / 2
    val ligneH: Image = blue(Rectangle(lg, ep))
    val ligneV: Image = blue(Rectangle(ep, lg))
    def tr(ligne: Image, x: Float, y: Float): Image =
      OnBackAt(ligne, layer, x, y)
    direction match
      case Nord  => tr(ligneV, offsetX, 0)
      case Sud   => tr(ligneV, offsetX, -lg)
      case Ouest => tr(ligneH, 0, offsetY)
      case Est   => tr(ligneH, -lg, offsetY)

  /** @param directions une liste de directions indiquant d'où ou bien
    *                   vers où un joueur est passé dans une cellule
    *
    * @return une image aux dimensions d'une cellule représentant la trace
    *         du passage du joueur dans une cellule, selon les directions données.
    *
    *         PHASE 1 : si la cellule est marquee Visitee(Nil), le dessin doit être une
    *         simple empreinte bleue (Cf. valeur empreinte).
    *
    * @example
    *  - si la cellule n'a jamais été visitée, l'image est un fond vide.
    *  - si le chemin du joueur ne passe qu'une fois par une cellule selon
    *    un trajet sud-nord, la liste `directions` reçues est alors
    *
    *    Sud :: Nord :: Nil
    *
    *    (ou bien Nord :: Sud :: Nil, l'ordre n'est pas important, car on ignore
    *    le sens de déplacement).
    *
    *    La trace du passage sera alors une ligne verticale traversant la cellule.
    */
  def trace(directions: List[Direction]): Image =
    if directions.isEmpty then calqueCellule(empreinte)
    else
      directions.foldRight(fond: Image)((direction, image) =>
        On(traceVers(direction), image)
      )

  /** @param etat l'état d'une cellule
    * @return une image aux dimensions d'une cellule représentant le
    *         tracé du passage du joueur selon l'état `etat`.
    */
  def calqueTrace(etat: EtatCellule): Image =
    val image = etat match
      case NonVisitee   => Empty
      case Courante(ds) => On(calqueCellule(joueur(ds)), trace(ds))
      case Visitee(ds)  => trace(ds)

    On(image, fond)

  /**  @note Un margeur est un motif placé dans une cellule pour indiquer
    *        l'entrée ou la sortie du labyrinthe.
    *
    *        Ici un marqueur est un calque opaque de couleur donnée
    *        à placer au fond, sous les autres calques éventuels.
    *
    *  @param couleur une couleur
    *  @return un calque avec la partie intérieure de la cellule remplie
    *          de la couleur spécifiée.
    */
  def calqueMarqueur(couleur: Color): Image =
    val motif: Image =
      colorie(couleur, square(taille_cellule - epaisseur_mur - 2))
    calqueCellule(motif)

  /** Une image de calqueMarqueur pour l'entrée d'un labyrinthe
    */
  val marqueurEntree: Image = calqueMarqueur(RED)

  /** Une image de calqueMarqueur pour la sortie d'un labyrinthe
    */
  val marqueurSortie: Image = calqueMarqueur(GREEN)

  /** @param cell une cellule
    * @param etat l'état de la cellule
    * @param optMarqueur un calque éventuel de marqueur à placer en fond d'image
    *        de la cellule, pour indiquer l'entrée ou la sortie d'un labyrinthe.
    *
    * @return une image représentant la cellule cell dans l'état etat, avec des
    *         murs le long des bords où les passages de cell sont fermés, et
    *         éventuellement le marqueur donné.
    */
  def celluleToImage(
      cell: Cellule,
      marqueur: Option[Image],
      etat: EtatCellule
  ): Image = {

    val traceJoueur = calqueTrace(etat)

    val traceEtMarqueur = marqueur match {
      case Some(img_marqueur) => On(traceJoueur, img_marqueur)
      case None               => traceJoueur
    }

    val img_cell = cell match
      case Cellule(Ferme, Ferme) =>
        OnAlign(
          Top,
          mur_horizontal,
          OnAlign(Right, traceEtMarqueur, mur_vertical)
        )
      case Cellule(Ferme, Ouvert) =>
        OnAlign(Top, traceEtMarqueur, mur_horizontal)
      case Cellule(Ouvert, Ferme) =>
        OnAlign(Right, traceEtMarqueur, mur_vertical)
      case Cellule(Ouvert, Ouvert) =>
        OnBackAt(jointure, traceEtMarqueur, epaisseur_mur - taille_cellule, 0)

    img_cell

  }

}