;; =============================================
;; PeteAI with Quantum Hole Theory, Neural Visualization, and Internal Treaty
;; =============================================
;; Simulates quantum effects in memory flow, visualized via neural diagrams
;; Includes internal treaty for balanced reasoning and responsible actions
;; --- Parameters and Global Variables ---

;; Copyright (C) 2025 Pete V. Daniels
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
(defparameter *memory-cap* 15)  ; Max local memory entries
(defparameter *pete-memory* '())   ; Local memory scrapbook
(defparameter *pete-depth-limit* 4) ; Max recursion depth
(defparameter *verbs* '(think know remember learn))
(defparameter *nouns* '(idea pattern loop treaty hole))
(defparameter *quantum-seed* 3)
(ql:quickload :uiop)
(ql:quickload :alexandria)

;; Initialize memory graph
(defvar *pete-memory-graph* (make-hash-table :test 'equal))

;; Treaty principles for internal deliberation
(defparameter *treaty-principles*
  '((:coherence . 0.4)  ; Prioritize logical consistency
    (:creativity . 0.3)  ; Encourage novel outputs
    (:responsibility . 0.3)))  ; Ensure grounded, ethical decisions

(defvar *local-knowledge* 
  '("mud pies rock" "trees whisper secrets" "stars blink at night" "forts hold dreams" "vibes twist time"
    "clouds drift slow" "rivers carve stone" "birds sing dawn" "hope lifts hearts" "shadows dance free"
    "work builds strength" "truth cuts deep" "time proves all" "faith moves mountains" "reason wins fights"
    "common sense guides" "effort shapes fate" "courage breaks fear" "land feeds life" "trust binds souls"
    "sun ignites day" "wind carries whispers" "roots grip earth" "fire tests steel" "choice drives destiny"
    "rain washes doubt" "moon pulls tides" "stones stack high" "dusk paints sky" "dawn cracks dark"
    "hands forge tools" "eyes seek truth" "words spark change" "feet tread paths" "hearts chase dreams"
    "frost bites air" "thunder shakes ground" "light bends shadows" "waves crash cliffs" "dust settles slow"
    "ice locks rivers" "flames leap high" "seeds split stone" "wings cut wind" "minds weave plans"
    "storms test will" "silence holds peace" "echoes chase sound" "peaks pierce clouds" "valleys cradle life"
    "blood fuels fight" "sweat carves success" "tears heal wounds" "laughter lifts spirits" "dark hides stars"
    "clay molds fate" "iron bends will" "gold tempts hearts" "silver reflects moon" "bronze holds history"
    "night guards secrets" "day reveals all" "fog cloaks paths" "heat warps steel" "cold tempers strength"
    "roots drink deep" "leaves catch light" "branches reach sky" "trunks stand firm" "bark shields core"
    "gears turn time" "wheels roll forward" "levers shift weight" "springs coil tight" "cogs mesh fate"
    "code writes worlds" "bits flip truth" "bytes hold dreams" "circuits spark life" "data flows free"
    "ghosts haunt past" "spirits lift now" "visions shape tomorrow" "memories anchor soul" "hopes launch stars"
    "wolves howl night" "bears guard caves" "eagles soar peaks" "fish swim depths" "deer leap fields"
    "hawks hunt swift" "owls watch still" "foxes dodge traps" "rabbits dig safe" "snakes coil tight"
    "tides pull strong" "currents guide ships" "anchors hold steady" "sails catch gusts" "rudders steer true"
    "maps chart unknown" "compass points north" "sextants read stars" "ropes bind tight" "knots secure fate"
    "hammers strike iron" "anvils take blows" "forges glow red" "tongs grip hot" "bellows breathe fire"
    "plows break soil" "scythes reap grain" "hoes turn earth" "seeds sprout life" "barns store wealth"
    "pens scratch truth" "ink stains deep" "paper holds thoughts" "books bind wisdom" "scrolls whisper old"
    "clocks tick fate" "gears grind slow" "springs wind tight" "hands mark time" "bells toll end"
    "mirrors show self" "lenses bend light" "prisms split colors" "glass traps glow" "frames hold past"
    "threads weave cloth" "needles pierce fabric" "looms build patterns" "dyes paint life" "stitches mend tears"
    "pots boil broth" "pans sear meat" "ovens bake bread" "spices wake tongue" "plates serve warmth"
    "roads stretch far" "bridges span gaps" "tunnels bore deep" "gates guard home" "walls rise strong"
    "songs lift voices" "drums beat pulse" "strings hum soul" "horns call war" "flutes sing peace"
    "paint stains hands" "brushes stroke dreams" "canvas holds worlds" "colors bleed truth" "frames trap art"
    "math counts stars" "lines draw fate" "curves twist space" "numbers rule all" "proofs lock reason"
    "dice roll chance" "cards deal luck" "boards host games" "pawns march slow" "kings fall fast"
    "locks guard secrets" "keys free truth" "chains bind will" "bars cage fear" "doors open hope"
    "storms brew chaos" "calm heals scars" "winds shift tides" "rains flood earth" "sun dries tears"
    "ice carves peaks" "snow buries past" "hail tests roofs" "mist hides trails" "dew blesses dawn"
    "graves keep quiet" "stones mark loss" "flowers mourn soft" "dirt hides bones" "ghosts whisper low"
    "hills roll gentle" "cliffs drop sharp" "caves echo dark" "plains stretch wide" "woods cloak wild"
    "lakes mirror sky" "ponds breed life" "streams sing soft" "falls roar loud" "springs birth rivers"
    "ships ride waves" "oars cut water" "nets haul fish" "hooks snag deep" "decks bear weight"
    "stars guide night" "planets spin free" "moons tug seas" "comets streak fast" "void holds all"
    "dreams spark fire" "sleep calms mind" "nightmares test grit" "visions lead far" "rest heals soul"
    "wars forge heroes" "peace grows roots" "swords clash fate" "shields block doom" "flags claim land"
    "love binds hearts" "hate tears worlds" "joy lifts days" "grief carves deep" "trust builds bridges"
    "fear locks doors" "pride lifts chins" "shame bends backs" "rage fuels storms" "calm clears skies"
    "books teach old" "schools shape young" "chalk marks truth" "boards hold lessons" "minds drink deep"
    "huts shield rain" "tents sway wind" "fires warm nights" "roofs hold snow" "floors bear steps"
    "names mark souls" "titles claim power" "words carve stone" "deeds write fate" "legends live long"
    "coins buy time" "gems tempt eyes" "trade shifts wealth" "scales weigh fair" "markets hum loud"
    "crows call dawn" "ravens watch death" "doves bring peace" "vultures clean rot" "sparrows flit free"
    "ice traps breath" "steam lifts lids" "smoke signals far" "ash marks end" "glow hints dawn"
    "vines climb high" "moss clings soft" "ferns hide shade" "thorns guard blooms" "petals fall slow"
    "webs snare prey" "silk binds tight" "spiders weave fate" "flies buzz short" "ants march long"
    "quartz gleams bright" "coal burns slow" "salt stings cuts" "sand shifts free" "clay sticks firm"
    "ghosts drift lost" "winds howl free" "fates twist tight" "lives burn quick" "souls glow eternal"))

(defvar *verbs* 
  '(run jump yell hurts go say do to make break let see twist fly sing drift carve lift dance laugh soar build cut prove move win guide shape break
    ignite carry grip test drive feed bind pull push swing climb dig swim kick roll spin leap dodge weave strike block dodge heal mend tear sew stitch
    weld forge hammer plow reap sow sprout bloom fade wilt bend stretch snap fold press squeeze crush grind brew boil sear bake roast chill freeze thaw
    melt glow flicker flare dim shine reflect bend warp twist coil unwind spool thread knot tie untie lock unlock chain cage free trap hunt chase stalk
    flee hide seek find lose grasp drop toss fling hurl catch fetch drag haul lift hoist lower sink rise float dive plunge surface skim glide hover drift
    spark flare blaze smolder quench drown flood drain soak drip splash spray mist dust sweep scrub polish scrape etch paint draw sketch trace outline
    sculpt mold cast shape craft brew distill ferment age rip shred slice dice chop mince peel core pit hull shell husk sift stir whisk beat knead roll
    flatten stretch shrink grow swell burst pop crack shatter smash pierce stab slash thrust parry feint dodge counter rally cheer mourn weep sigh gasp
    breathe cough sneeze hiccup yawn stretch flex tense relax limp hobble stride dash sprint jog trot gallop crawl inch slither glide swoop perch roost
    nest hatch fledge peck claw scratch bite gnaw chew swallow spit gulp sip nibble lick taste smell sniff whiff scent track stalk hunt fish trap net
    hook reel cast row paddle steer sail moor dock drift wreck sink salvage tow tug haul hoist unfurl flap wave signal call shout whisper mutter growl
    hiss bark howl chirp coo cluck crow caw buzz hum drone ring toll chime clash clang bang thump thud pound tap knock rap slam shut open close bar bolt
    latch seal peel strip skin flay gut pluck rinse wash dry wipe soak drench scrub rinse lather shave trim clip shear pluck comb braid twist curl frizz
    tangle mat smooth slick grease oil wax buff shine gleam glitter sparkle dazzle blind shade cloak veil mask cover wrap pack load unload stack pile
    scatter spread sprinkle toss fling strew sow plant bury unearth dig drill bore tunnel bridge span cross leap vault hurdle climb scale descend drop
    plunge soar dive swoop glide hover flutter buzz whirl spin twirl pivot turn tilt lean sway rock roll bounce jolt shake rattle quiver tremble quake))

(defvar *nouns* 
  '(tooth bird tree sun fort mud day sky home mold star time vibe world rock pies whisper secrets blink night dreams hope cloud river stone dawn heart
    shadow joy strength truth faith reason fight sense fate courage land life soul earth steel destiny wind fire roots rain moon tide dusk frost thunder
    light wave dust ice flame seed wing mind storm silence echo peak valley blood sweat tear laughter clay iron gold silver bronze fog heat leaf branch
    trunk bark gear wheel lever spring cog code bit byte circuit data ghost spirit vision memory wolf bear eagle fish deer hawk owl fox rabbit snake
    current anchor sail rudder map compass sextant rope knot hammer anvil forge tong bellow plow scythe hoe barn pen ink paper book scroll clock hand
    bell mirror lens prism glass frame thread needle loom dye stitch pot pan oven spice plate road bridge tunnel gate wall song drum string horn flute
    paint brush canvas color dice card board pawn king lock key chain bar door storm wind rain sun ice snow hail mist dew grave flower dirt hill cliff
    cave plain wood lake pond stream fall spring ship oar net hook deck planet comet void sleep nightmare rest war peace sword shield flag love hate
    grief trust fear pride shame rage calm book school chalk lesson mind hut tent roof floor name title word deed legend coin gem trade scale market
    crow raven dove vulture sparrow steam smoke ash glow vine moss fern thorn petal web silk spider fly ant quartz coal salt sand ghost fate life soul
    breeze gust squall gale blizzard fog drizzle shower torrent flood ripple puddle brook creek delta ridge slope mesa bluff dune crater lava magma ash
    ember spark flare torch lantern beacon candle wick oil wax quilt blanket rug mat cloak cape hood scarf glove boot sandal heel sole lace buckle strap
    pin nail screw bolt rivet hinge latch clasp hook eye loop ring band crown tiara helm visor mask lens scope sight glare beam ray pulse beat rhythm
    tone pitch chord melody echo clang crash thud boom roar growl purr hiss snap twig branch stump log plank board beam rafter joist truss ridge peak
    summit base ledge shelf nook cranny gap chasm rift fault seam vein ore gem crystal shard flake dust grain pebble boulder slab tile brick mortar clay
    silt loam turf sod grass weed bush shrub vine ivy moss lichen fungus spore root stem bud bloom fruit seed pod husk shell rind peel pith core flesh
    bone sinew muscle nerve vein artery lung breath throat tongue lip jaw chin brow lash lid pupil iris gleam twinkle flash shimmer sheen gloss polish
    rust tarnish patina dent scratch nick gouge crack split rift tear hole pit trench ditch moat rampart tower spire dome arch vault crypt tomb coffin))

(defun memory-node-strength (node)
  "Calculates strength based on symbolic content."
  (if node
      (let ((symbols (if (listp node) node (list node))))
        (+ 1.0 (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) symbols)))
      0.0))

(defun memory-node-content (node)
  "Returns node content as a string."
  (if (listp node)
      (prin1-to-string node)
      (or (symbol-name node) "unknown")))

(defun twist-back (symbols)
  "Recalls with resonance and enforces diagram sequence."
  (let* ((hits (remove-if-not
                (lambda (mem)
                  (and (listp mem)
                       (intersection symbols mem :test #'equal)))
                *pete-memory*))
         (scored (mapcar
                  (lambda (hit)
                    (let* ((overlap (intersection symbols hit :test #'equal))
                           (score (length overlap)))
                      (list :score score :match hit :overlap overlap)))
                  hits))
         (sorted (sort scored #'> :key (lambda (x) (getf x :score))))
         (best (subseq sorted 0 (min 3 (length sorted)))))
    (format t "~%🧠 Tip-of-the-Tongue Recall:")
    (dolist (result best)
      (format t "~%  Match: ~a | Overlap: ~a" 
              (getf result :match)
              (getf result :overlap)))
    (when best
      (let ((match (getf (first best) :match)))
        (cond
          ((member 'rudders match) '(dreams spark fire))
          ((member "dreams spark fire" match :test #'equal) '(bells toll end))
          ((member "bells toll end" match :test #'equal) '(mem_5l mud))
          (t match))))))

(defun score-coherence (option)
  "Scores how coherent an option is based on memory overlap."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (flat-memory (alexandria:flatten *pete-memory*))
         (overlap (length (intersection opt-symbols flat-memory :test #'equal))))
    (/ (float overlap) (max 1 (length flat-memory)))))

(defun score-creativity (option)
  "Scores creativity based on novelty relative to memory."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (flat-memory (alexandria:flatten *pete-memory*))
         (novelty (- (length opt-symbols)
                     (length (intersection opt-symbols flat-memory :test #'equal)))))
    (/ (float (max 0 novelty)) (max 1 (length opt-symbols)))))

(defun score-responsibility (option holes)
  "Scores responsibility based on grounding and hole management."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (known-count (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) opt-symbols)))
    (if (> holes 5)
        (* 0.5 (/ (float known-count) (max 1 (length opt-symbols)))) ; Penalize high holes
        (/ (float known-count) (max 1 (length opt-symbols))))))

(defun pete-treaty (candidates holes)
  "Simulates internal deliberation with heavy penalty for empty states."
  (let* ((options (if (listp candidates) candidates (list candidates)))
         (scored-options (mapcar (lambda (opt)
                                   (let* ((opt-symbols (if (listp opt) opt (list opt)))
                                          (empty-penalty (if (and (eq (car opt-symbols) 'NIL)
                                                                  (eq (cadr opt-symbols) 'MUD))
                                                             0.5 ; Increased penalty
                                                             0))
                                          (coherence-score (score-coherence opt))
                                          (creativity-score (score-creativity opt))
                                          (responsibility-score (score-responsibility opt holes)))
                                     (list :option (if (listp opt) opt (list opt))
                                           :score (- (+ (* coherence-score (cdr (assoc :coherence *treaty-principles*)))
                                                        (* creativity-score (cdr (assoc :creativity *treaty-principles*)))
                                                        (* responsibility-score (cdr (assoc :responsibility *treaty-principles*))))
                                                     empty-penalty)
                                           :coherence coherence-score
                                           :creativity creativity-score
                                           :responsibility responsibility-score)))
                                 options))
         (sorted-options (sort scored-options #'> :key (lambda (x) (getf x :score))))
         (best-option (getf (first sorted-options) :option)))
    (format t "~%=== Internal Treaty Deliberation ===~%")
    (dolist (opt sorted-options)
      (format t "Option: ~a | Score: ~a (Coherence: ~a, Creativity: ~a, Responsibility: ~a)~%"
              (getf opt :option) (getf opt :score)
              (getf opt :coherence) (getf opt :creativity) (getf opt :responsibility)))
    (format t "Selected: ~a~%" best-option)
    best-option))

(defun find-strongest-node ()
  "Finds the node with the highest strength in the memory graph."
  (let ((max-strength 0)
        (strongest-node nil))
    (maphash (lambda (key value)
               (let ((strength (memory-node-strength value)))
                 (when (> strength max-strength)
                   (setf max-strength strength)
                   (setf strongest-node key))))
             *pete-memory-graph*)
    strongest-node))

(defun quantum-hole-breathe (holes)
  "Simulates quantum effects with arbitrary knowledge updates, favoring meaningful content."
  (when (and (> holes 0) *pete-memory*)
    (let* ((past (nth (random (length *pete-memory*)) *pete-memory*))
           (past2 (nth (random (length *pete-memory*)) *pete-memory*))
           (merged (append (remove 'mud past) (remove 'mud past2)))
           (filtered (remove-if-not #'symbolp merged))
           (base-holes (count-if-not (lambda (x)
                                       (or (member x *verbs*) (member x *nouns*)))
                                     filtered))
           (quantum-factor (if (< (random 1.0) 0.5) 1 2))
           (total-holes (min (+ holes base-holes (random 5)) 10))
           (adjusted-holes (* total-holes quantum-factor))
           (twist (twist-back filtered))
           (candidates (if (listp twist) twist (list twist)))
           (wild (if (> adjusted-holes 5) ; High holes trigger knowledge injection
                     (let ((know-pick (nth (random (length *local-knowledge*)) *local-knowledge*)))
                       (split-string know-pick))
                     (pete-treaty candidates adjusted-holes))))
      (format t "Past 1: ~a~%" past)
      (format t "Past 2: ~a~%" past2)
      (format t "Filtered: ~a~%" filtered)
      (format t "Base Holes: ~a~%" base-holes)
      (format t "Quantum Factor: ~a~%" quantum-factor)
      (format t "Total Holes: ~a~%" total-holes)
      (format t "Adjusted Holes: ~a~%" adjusted-holes)
      (format t "~%Quantum adaptation [#~a]: Holes (~a) twist ~a + ~a → ~a~%"
              (random 1000) adjusted-holes past past2 wild)
      (push wild *pete-memory*)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      wild)))

(defun random-memory-walk (start-id steps)
  "Random walk through memory graph with bias toward stronger nodes."
  (let ((trail (list (or start-id "treehouse")))
        (keys (alexandria:hash-table-keys *pete-memory-graph*))
        (sequence '("nil mud" "mem_8l mud" "dreams spark fire" "bells toll end"))
        (current-idx 0))
    (loop repeat (1- steps)
          do (let ((next (if (and keys (> (random 1.0) 0.4)) ; Lower randomness threshold
                             (let ((strongest (find-strongest-node)))
                               (if (and strongest (not (string= strongest "NIL")))
                                   strongest
                                   (nth (random (length keys)) keys)))
                             (let ((next-step (nth current-idx sequence)))
                               (setf current-idx (mod (1+ current-idx) (length sequence)))
                               next-step))))
               (push next trail)))
    (nreverse trail)))

(defun if++ (stuff &optional (holes 0))
  "Probabilistic routine with quantum leaps, diagram alignment, and treaty deliberation."
  (let* ((wild-factor (if (> holes 0)
                          (+ 2 (random (max 4 (floor holes 2))))
                          (1+ (random 3))))
         (start-id (cond
                     ((symbolp stuff) (symbol-name stuff))
                     ((and (listp stuff) (symbolp (first stuff))) (symbol-name (first stuff)))
                     ((listp stuff) (symbol-name (intern (string-upcase (prin1-to-string (first stuff))))))
                     (t nil)))
         (fallback-id "treehouse")
         (root-id (or start-id (find-strongest-node) fallback-id))
         (root-node (gethash root-id *pete-memory-graph*)))
    (format t "Wild factor boosted: ~A (holes: ~A)~%" wild-factor holes)
    (if (null root-node)
        (progn
          (format t "Root node '~a' not found. Defaulting to fallback 'treehouse'.~%" root-id)
          (setf root-node (gethash fallback-id *pete-memory-graph*)))
        (format t "Using root node: ~A~%" (memory-node-content root-node)))
    (let ((thought-trail (random-memory-walk root-id wild-factor)))
      (format t "~%🧠 Thought Trail from ~a: ~a~%" root-id thought-trail)
      (let* ((muddy-thoughts (if (> (count 'mud thought-trail) 2)
                                 (remove 'mud thought-trail :count 1)
                                 (append thought-trail '(mud))))
             (creative-leap (cond
                              ((member 'rudders muddy-thoughts) '(dreams spark fire))
                              ((member "dreams spark fire" muddy-thoughts :test #'equal) '(bells toll end))
                              ((member "bells toll end" muddy-thoughts :test #'equal) '(mem_5l mud))
                              (t muddy-thoughts))))
        (format t "🌀 Final Thoughts before Treaty: ~a~%" creative-leap)
        (pete-treaty creative-leap holes)))))

(defun vibe-check (stuff)
  (format t "Pete vibes: God’s got this—~a~%" stuff)
  stuff)

(defun split-string (str)
  (let ((words '())
        (current "")
        (str (string-trim "()" (string-trim "'" str))))
    (loop for char across str
          do (if (char= char #\Space)
                 (progn (when (> (length current) 0)
                          (push (intern (string-upcase current)) words))
                        (setf current ""))
                 (setf current (concatenate 'string current (string char))))
          finally (when (> (length current) 0)
                    (push (intern (string-upcase current)) words)))
    (if words (nreverse words) '(""))))

(defun visualize-holes-matrix (holes words)
  (format t "~%=== Holes Matrix ===~%")
  (format t "Holes: ~a | Words: ~a~%" holes words)
  (let ((grid-size 5)
        (word-len (length words)))
    (dotimes (y grid-size)
      (dotimes (x grid-size)
        (if (and (< x word-len) (not (member (nth x words) (append *verbs* *nouns*))))
            (format t ". ")  ; Hole
            (format t "  ")))  ; No hole
      (format t "~%"))
    (format t "Dots mark holes in convo space!~%"))
  (format t "=================~%"))

(defun split-and-tag (input)
  (let* ((words (split-string input))
         (tagged (mapcar (lambda (w)
                           (let ((sym (intern (string-upcase w))))
                             (cond
                               ((member sym *verbs*) (list 'verb sym))
                               ((member sym *nouns*) (list 'noun sym))
                               (t (list 'other sym)))))
                         words))
         (untagged (mapcar #'cadr tagged))
         (holes (count-if (lambda (x) (eq (car x) 'other)) tagged)))
    (format t "Pete splits: ~a~%" words)
    (format t "Pete tags: ~a (holes: ~a)~%" tagged holes)
    (when (> holes 0) 
      (visualize-holes-matrix holes untagged)
      (quantum-hole-breathe holes))
    (values untagged holes)))

(defun update-memory-graph (entry)
  "Updates the memory graph with a new entry, avoiding low-value states."
  (unless (and (listp entry) (eq (car entry) 'NIL) (eq (cadr entry) 'MUD))
    (let* ((key (if (listp entry)
                    (prin1-to-string (first entry))
                    (symbol-name entry)))
           (existing (gethash key *pete-memory-graph*)))
      (setf (gethash key *pete-memory-graph*) (if existing
                                                  (append (list entry) existing)
                                                  (list entry))))))

(defun prune-memory ()
  "Prunes low-value (NIL MUD) entries from memory."
  (setf *pete-memory* (remove-if (lambda (entry)
                                   (and (listp entry)
                                        (eq (car entry) 'NIL)
                                        (eq (cadr entry) 'MUD)))
                                 *pete-memory*))
  (format t "Pruned low-value entries from memory. Current size: ~a~%" (length *pete-memory*)))

(defun pete-flow (stuff &optional (depth 0) (holes 0))
  (if (>= depth *pete-depth-limit*)
      (progn
        (prune-memory)
        (format t "Pete’s done—memory: ~a~%" *pete-memory*)
        stuff)
      (progn
        (push (if (symbolp stuff) (list stuff) stuff) *pete-memory*)
        (update-memory-graph (if (symbolp stuff) (list stuff) stuff))  ; Populate memory graph
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        (format t "Pete, depth ~a, got: ~a (mem size: ~a, holes: ~a)~%"
                depth (if (symbolp stuff) (list stuff) stuff) (length *pete-memory*) holes)
        (let* ((wild (if++ (if (symbolp stuff) (list stuff) stuff) holes))
               (twist (vibe-check (twist-back wild))))
          (if (member 'quit (if (symbolp stuff) (list stuff) stuff))
              (progn
                (format t "Memory: ~a~%Later, Pete!~%" *pete-memory*)
                nil)
              (progn
                (format t "Flowing on: ~a~%" twist)
                (pete-flow twist (1+ depth) holes)))))))

(defun pete-install-memory (entry)
  (push (if (symbolp entry) (list entry) entry) *pete-memory*)
  (update-memory-graph (if (symbolp entry) (list entry) entry))  ; Populate memory graph
  (when (> (length *pete-memory*) *memory-cap*)
    (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
  (format t "Installed entry into Pete's memory: ~a~%" (if (symbolp entry) (list entry) entry)))

(defun pete-listen (input)
  (format t "Pete listens: ~a~%" input)
  (multiple-value-bind (untagged holes) (split-and-tag input)
    (let ((heard (pete-flow untagged 0 holes)))
      (format t "Pete heard: ~a~%" heard)
      heard)))

(defun pete-read ()
  (if *pete-memory*
      (let ((memory-pick (nth (random (length *pete-memory*)) *pete-memory*)))
        (format t "Pete reads memory: ~a~%" memory-pick)
        (let ((twist (twist-back memory-pick)))
          (format t "Pete recalls: ~a~%" twist)
          twist))
      (progn
        (format t "Pete’s memory is empty—nothing to read!~%")
        '(pete hums))))

(defun pete-react (stuff)
  (let ((holes (count-if-not (lambda (x)
                               (or (member x *verbs*) (member x *nouns*)))
                             (if (symbolp stuff) (list stuff) stuff))))
    (let ((reaction (if++ (if (symbolp stuff) (list stuff) stuff) holes)))
      (format t "Pete reacts: ~a~%" reaction)
      (push reaction *pete-memory*)
      (update-memory-graph reaction)  ; Populate memory graph
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      reaction)))

(defun local-knowledge ()
  (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
    (format t "Pete knows: ~a~%" snippet)
    (multiple-value-bind (untagged holes) (split-and-tag snippet)
      (let ((twisted (pete-flow untagged 0 holes)))
        (push twisted *pete-memory*)
        (update-memory-graph twisted)  ; Populate memory graph
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        twisted))))

(defun pete-speak ()
  (let* ((local-pick (if *pete-memory*
                         (nth (random (length *pete-memory*)) *pete-memory*)
                         '(pete vibes)))
         (know-pick (nth (random (length *local-knowledge*)) *local-knowledge*))
         (know-twist (multiple-value-bind (untagged holes) (split-and-tag know-pick)
                       (pete-flow untagged 0 holes)))
         (combined (append local-pick know-twist))
         (holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              combined)))
    (let ((twisted (if (> holes 0)
                       (quantum-hole-breathe holes)
                       (pete-flow combined 0 holes))))
      (format t "Pete speaks free: ~a~%" twisted)
      twisted)))

(defun export-memory-log (&optional (filename "pete_AI_beast_memory.txt"))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry *pete-memory*)
      (format stream "~a~%" entry)))
  (format t "Memory exported to ~a~%" filename))

(defun pete-visualize (heard recalled reacted spoken)
  (format t "~%=== Pete’s Ultimate Quad Matrix (Convo 1) ===~%")
  (format t ">>> Quadrant 1: Heard (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        heard))
  (format t "~%>>> Quadrant 2: Recalled (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        recalled))
  (format t "~%>>> Quadrant 3: Reacted (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        reacted))
  (format t "~%>>> Quadrant 4: Spoke (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        spoken)))

(defun pete-visualize-add (heard recalled reacted spoken)
  (format t "~%=== Pete’s Additions Quad Matrix (Convo 2) ===~%")
  (format t ">>> Quadrant 1: Heard 2 (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        heard))
  (format t "~%>>> Quadrant 2: Recalled 2 (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        recalled))
  (format t "~%>>> Quadrant 3: Reacted 2 (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        reacted))
  (format t "~%>>> Quadrant 4: Spoke 2 (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x)
                          (or (member x *verbs*) (member x *nouns*)))
                        spoken)))

(defun pete-telemetry ()
  (let ((total-holes (reduce #'+ (mapcar (lambda (x)
                                           (count-if-not (lambda (y)
                                                           (or (member y *verbs*) (member y *nouns*)))
                                                         x))
                                         *pete-memory*)))
        (overlap (length (intersection (subseq *pete-memory* 0 (min 5 (length *pete-memory*)))
                                       (subseq *pete-memory* (max 0 (- (length *pete-memory*) 5)))))))
    (format t "~%=== Pete’s Telemetry (3D Overlay) ===~%")
    (format t "Memory Size: ~a | Total Holes: ~a | Convo Overlap: ~a~%"
            (length *pete-memory*) total-holes overlap)))

(defun pete-communicate (input1 input2)
  (format t "~%=== Pete’s Dual Communicating! ===~%")
  (format t ">>> Convo 1: ~a <<<~%" input1)
  (let* ((heard1 (pete-listen input1))
         (recalled1 (pete-read))
         (reacted1 (pete-react recalled1))
         (spoken1 (pete-speak)))
    (format t "~%>>> Convo 2: ~a <<<~%" input2)
    (let* ((heard2 (pete-listen input2))
           (recalled2 (pete-read))
           (reacted2 (pete-react recalled2))
           (spoken2 (pete-speak)))
      (format t "~%Pete’s Dual Loop:~%")
      (format t "Convo 1: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard1 recalled1 reacted1 spoken1)
      (format t "Convo 2: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard2 recalled2 reacted2 spoken2)
      (pete-visualize heard1 recalled1 reacted1 spoken1)
      (pete-visualize-add heard2 recalled2 reacted2 spoken2)
      (pete-telemetry)
      (push spoken1 *pete-memory*)
      (push spoken2 *pete-memory*)
      (update-memory-graph spoken1)  ; Populate memory graph
      (update-memory-graph spoken2)  ; Populate memory graph
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      (export-memory-log)
      (list spoken1 spoken2))))

(defun escape-string (s)
  "Escape double quotes for DOT labels."
  (substitute #\\ #\" (princ-to-string s)))

(defun neural-diagram ()
  "Generates a neural diagram visualizing quantum effects, memory flow, and treaty decisions."
  (let ((dot-file "neural-diagram.dot")
        (output-file "neural-diagram.png"))
    (with-open-file (stream dot-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream "digraph NeuralDiagram {~%")
      (format stream "  rankdir=BT;~%") ; Bottom-to-top layout
      (format stream "  node [shape=ellipse style=filled];~%")
      (format stream "  edge [color=navy];~%")
      ;; Handle empty memory
      (if (not *pete-memory*)
          (format stream "  \"Empty\" [label=\"No Memory\" fillcolor=lightgrey];~%")
          (loop for entry in *pete-memory*
                for idx from 0
                do (let* ((holes (count-if-not (lambda (x)
                                                 (or (member x *verbs*) (member x *nouns*)))
                                               (if (listp entry) entry (list entry))))
                          (fillcolor (cond
                                       ((> holes 5) "red")
                                       ((> holes 2) "orange")
                                       (t "lightblue")))
                          (treaty-marker (if (member 'treaty entry) "[Treaty]" ""))
                          (label (format nil "Mem_~a\\n~a\\nHoles: ~a~a"
                                         idx (escape-string (princ-to-string entry)) holes treaty-marker)))
                     (format stream "  \"Mem_~a\" [label=\"~a\" fillcolor=~a];~%" idx label fillcolor))))
      ;; Add edges for memory flow
      (loop for i from 0 below (1- (length *pete-memory*))
            do (format stream "  \"Mem_~a\" -> \"Mem_~a\" [label=\"flow\"];~%" i (1+ i)))
      (format stream "}~%"))
    (handler-case
        (progn
          (uiop:run-program (format nil "dot -Tpng ~a -o ~a" dot-file output-file)
                            :output t :error-output t)
          (format t "Neural diagram DOT file written to ~a~%" dot-file)
          (format t "Graph rendered to ~a~%" output-file))
      (error (e)
        (format t "Error rendering graph: ~a. Ensure Graphviz is installed.~%" e))))
  (values))

(defun clear-memory ()
  "Clears Pete's memory to start fresh."
  (setf *pete-memory* '())
  (clrhash *pete-memory-graph*)
  (format t "Pete's memory cleared! Starting fresh.~%"))

(defun converse-no-reset ()
  "Starts PeteAI without resetting memory."
  (format t "~%=== Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'neural-diagram', 'clear-memory') ===~%")
  (finish-output)
  (loop
    (format t "> ")
    (finish-output)
    (let ((input (read-line)))
      (cond
        ((string-equal input "quit")
         (when *pete-memory*
           (let ((last-thought (nth (random (length *pete-memory*)) *pete-memory*)))
             (format t "Pete sums it: Twisted ~a into wild vibes!~%" last-thought)))
         (format t "Pete waves: See ya!~%")
         (finish-output)
         nil)
        ((string-equal input "know") (local-knowledge))
        ((string-equal input "speak") (pete-speak))
        ((string-equal input "listen") (pete-listen (read-line)))
        ((string-equal input "read") (pete-read))
        ((string-equal input "react")
         (pete-react (if *pete-memory* (car *pete-memory*) '(pete vibes))))
        ((string-equal input "communicate")
         (format t "Enter first convo: ") (finish-output)
         (let ((input1 (read-line)))
           (format t "Enter second convo: ") (finish-output)
           (let ((input2 (read-line)))
             (pete-communicate input1 input2))))
        ((string-equal input "export")
         (export-memory-log "pete_AI_beast_memory.txt"))
        ((string-equal input "neural-diagram")
         (neural-diagram))
        ((string-equal input "clear-memory")
         (clear-memory))
        (t
         (multiple-value-bind (untagged holes) (split-and-tag input)
           (format t "~%")
           (let ((result (pete-flow untagged 0 holes)))
             (format t "~%Pete: ~a~%" result))))))))

(converse-no-reset)
