;; =============================================
;; Replicate PeteAI with Quantum Hole Theory
;; File: Groks_Qua_Qub_Tr_12bk_4_26_25.lisp (from bk6 April 24th 2025)
;; Date: April 24, 2025
;; Author: Pete (with Grok’s & Chats cosmic assist)
;; =============================================

;; --- Dependencies ---
(ql:quickload :uiop :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :usocket :silent t)
(ql:quickload :bordeaux-threads :silent t)
(handler-case
    (ql:quickload "parse-number" :silent t)
  (error (e)
    (format t "Error loading parse-number: ~a~%Please install parse-number via Quicklisp.~%" e)
    (sb-ext:exit :code 1)))

;; --- Parameters and Global Variables ---
(defparameter *memory-cap* 10)  ; Max local memory entries
(defparameter *pete-memory* '())  ; Local memory scrapbook
(defparameter *pete-depth-limit* 4)  ; Max recursion depth
(defparameter *verbs* '(think know remember learn))
(defparameter *nouns* '(idea pattern loop treaty hole))
(defparameter *quantum-seed* 3)
(defvar *pete-memory-graph* (make-hash-table :test 'equal))  ; Memory graph
(defvar *pete-treaty-map* (make-hash-table :test 'equal))  ; Treaty storage
(defparameter *current-mode* :auto)  ; Track the current mode for dynamic switching

;; Seed the random number generator to prevent hangs
(setf *random-state* (make-random-state t))



(defvar *local-knowledge*
  '("mud pies rock" "trees whisper secrets" "stars blink at night" "forts hold dreams"
    "vibes twist time" "clouds drift slow" "rivers carve stone" "birds sing dawn"
    "hope lifts hearts" "shadows dance free" "work builds strength" "truth cuts deep"
    "time proves all" "faith moves mountains" "reason wins fights" "common sense guides"
    "effort shapes fate" "courage breaks fear" "land feeds life" "trust binds souls"
    "sun ignites day" "wind carries whispers" "roots grip earth" "fire tests steel"
    "choice drives destiny" "rain washes doubt" "moon pulls tides" "stones stack high"
    "dusk paints sky" "dawn cracks dark" "hands forge tools" "eyes seek truth"
    "words spark change" "feet tread paths" "hearts chase dreams" "frost bites air"
    "thunder shakes ground" "light bends shadows" "waves crash cliffs" "dust settles slow"
    "ice locks rivers" "flames leap high" "seeds split stone" "wings cut wind"
    "minds weave plans" "storms test will" "silence holds peace" "echoes chase sound"
    "peaks pierce clouds" "valleys cradle life" "blood fuels fight" "sweat carves success"
    "tears heal wounds" "laughter lifts spirits" "dark hides stars" "clay molds fate"
    "iron bends will" "gold tempts hearts" "silver reflects moon" "bronze holds history"
    "night guards secrets" "day reveals all" "fog cloaks paths" "heat warps steel"
    "cold tempers strength" "roots drink deep" "leaves catch light" "branches reach sky"
    "trunks stand firm" "bark shields core" "gears turn time" "wheels roll forward"
    "levers shift weight" "springs coil tight" "cogs mesh fate" "code writes worlds"
    "bits flip truth" "bytes hold dreams" "circuits spark life" "data flows free"
    "ghosts haunt past" "spirits lift now" "visions shape tomorrow" "memories anchor soul"
    "hopes launch stars" "wolves howl night" "bears guard caves" "eagles soar peaks"
    "fish swim depths" "deer leap fields" "hawks hunt swift" "owls watch still"
    "foxes dodge traps" "rabbits dig safe" "snakes coil tight" "tides pull strong"
    "currents guide ships" "anchors hold steady" "sails catch gusts" "rudders steer true"
    "maps chart unknown" "compass points north" "sextants read stars" "ropes bind tight"
    "knots secure fate" "hammers strike iron" "anvils take blows" "forges glow red"
    "tongs grip hot" "bellows breathe fire" "plows break soil" "scythes reap grain"
    "hoes turn earth" "seeds sprout life" "barns store wealth" "pens scratch truth"
    "ink stains deep" "paper holds thoughts" "books bind wisdom" "scrolls whisper old"
    "clocks tick fate" "gears grind slow" "springs wind tight" "hands mark time"
    "bells toll end" "mirrors show self" "lenses bend light" "prisms split colors"
    "glass traps glow" "frames hold past" "threads weave cloth" "needles pierce fabric"
    "looms build patterns" "dyes paint life" "stitches mend tears" "pots boil broth"
    "pans sear meat" "ovens bake bread" "spices wake tongue" "plates serve warmth"
    "roads stretch far" "bridges span gaps" "tunnels bore deep" "gates guard home"
    "walls rise strong" "songs lift voices" "drums beat pulse" "strings hum soul"
    "horns call war" "flutes sing peace" "paint stains hands" "brushes stroke dreams"
    "canvas holds worlds" "colors bleed truth" "frames trap art" "math counts stars"
    "lines draw fate" "curves twist space" "numbers rule all" "proofs lock reason"
    "dice roll chance" "cards deal luck" "boards host games" "pawns march slow"
    "kings fall fast" "locks guard secrets" "keys free truth" "chains bind will"
    "bars cage fear" "doors open hope" "storms brew chaos" "calm heals scars"
    "winds shift tides" "rains flood earth" "sun dries tears" "ice carves peaks"
    "snow buries past" "hail tests roofs" "mist hides trails" "dew blesses dawn"
    "graves keep quiet" "stones mark loss" "flowers mourn soft" "dirt hides bones"
    "ghosts whisper low" "hills roll gentle" "cliffs drop sharp" "caves echo dark"
    "plains stretch wide" "woods cloak wild" "lakes mirror sky" "ponds breed life"
    "streams sing soft" "falls roar loud" "springs birth rivers" "ships ride waves"
    "oars cut water" "nets haul fish" "hooks snag deep" "decks bear weight"
    "stars guide night" "planets spin free" "moons tug seas" "comets streak fast"
    "void holds all" "dreams spark fire" "sleep calms mind" "nightmares test grit"
    "visions lead far" "rest heals soul" "wars forge heroes" "peace grows roots"
    "swords clash fate" "shields block doom" "flags claim land" "love binds hearts"
    "hate tears worlds" "joy lifts days" "grief carves deep" "trust builds bridges"
    "fear locks doors" "pride lifts chins" "shame bends backs" "rage fuels storms"
    "calm clears skies" "books teach old" "schools shape young" "chalk marks truth"
    "boards hold lessons" "minds drink deep" "huts shield rain" "tents sway wind"
    "fires warm nights" "roofs hold snow" "floors bear steps" "names mark souls"
    "titles claim power" "words carve stone" "deeds write fate" "legends live long"
    "coins buy time" "gems tempt eyes" "trade shifts wealth" "scales weigh fair"
    "markets hum loud" "crows call dawn" "ravens watch death" "doves bring peace"
    "vultures clean rot" "sparrows flit free" "ice traps breath" "steam lifts lids"
    "smoke signals far" "ash marks end" "glow hints dawn" "vines climb high"
    "moss clings soft" "ferns hide shade" "thorns guard blooms" "petals fall slow"
    "webs snare prey" "silk binds tight" "spiders weave fate" "flies buzz short"
    "ants march long" "quartz gleams bright" "coal burns slow" "salt stings cuts"
    "sand shifts free" "clay sticks firm" "ghosts drift lost" "winds howl free"
    "fates twist tight" "lives burn quick" "souls glow eternal"))

(defvar *verbs*
  '(be is are was were go went gone run ran runs
    jump jumped jumps speak spoke spoken know knew known
    think thought thoughts vibe vibes vibing whisper whispered
    whispers carry carries carried ignite ignites ignited
    grip grips gripped test tests tested sing sang sung
    dance danced dances lift lifts lifted build builds built
    cut cuts cutting break breaks broke shape shapes shaped
    feed feeds fed bind binds bound prove proves proved
    move moves moved win wins won guide guides guided run jump yell hurts go
    say do to make break let see twist fly sing drift carve lift
    dance laugh soar build cut prove move win guide shape break ignite carry grip test
    drive feed bind pull push swing climb dig swim kick roll spin leap dodge weave
    strike block dodge heal mend tear sew stitch weld forge hammer plow reap sow sprout
    bloom fade wilt bend stretch snap fold press squeeze crush grind brew boil sear bake
    roast chill freeze thaw melt glow flicker flare dim shine reflect bend warp twist
    coil unwind spool thread knot tie untie lock unlock chain cage free trap hunt chase
    stalk flee hide seek find lose grasp drop toss fling hurl catch fetch drag haul lift
    hoist lower sink rise float dive plunge surface skim glide hover drift spark flare
    blaze smolder quench drown flood drain soak drip splash spray mist dust sweep scrub
    polish scrape etch paint draw sketch trace outline sculpt mold cast shape craft brew
    distill ferment age rip shred slice dice chop mince peel core pit hull shell husk
    sift stir whisk beat knead roll flatten stretch shrink grow swell burst pop crack
    shatter smash pierce stab slash thrust parry feint dodge counter rally cheer mourn
    weep sigh gasp breathe cough sneeze hiccup yawn stretch flex tense relax limp hobble
    stride dash sprint jog trot gallop crawl inch slither glide swoop perch roost nest
    hatch fledge peck claw scratch bite gnaw chew swallow spit gulp sip nibble lick
    taste smell sniff whiff scent track stalk hunt fish trap net hook reel cast row
    paddle steer sail moor dock drift wreck sink salvage tow tug haul hoist unfurl flap
    wave signal call shout whisper mutter growl hiss bark howl chirp coo cluck crow caw
    buzz hum drone ring toll chime clash clang bang thump thud pound tap knock rap slam
    shut open close bar bolt latch seal peel strip skin flay gut pluck rinse wash dry
    wipe soak drench scrub rinse lather shave trim clip shear pluck comb braid twist
    curl frizz tangle mat smooth slick grease oil wax buff shine gleam glitter sparkle
    dazzle blind shade cloak veil mask cover wrap pack load unload stack pile scatter
    spread sprinkle toss fling strew sow plant bury unearth dig drill bore tunnel bridge
    span cross leap vault hurdle climb scale descend drop plunge soar dive swoop glide
    hover flutter buzz whirl spin twirl pivot turn tilt lean sway rock roll bounce jolt
    shake rattle quiver tremble quake))

(defvar *nouns*
  '(sun wind roots fire mud pies trees stars forts vibes
    clouds rivers birds hope shadows work truth time faith
    reason sense effort courage land trust day whispers earth
    steel secrets night dreams stone dawn hearts strength fate
    fear life souls mountains fights guides leavestooth bird tree sun fort mud
    day sky home mold star time vibe world rock pies
    whisper secrets blink night dreams hope cloud river stone dawn heart shadow joy
    strength truth faith reason fight sense fate courage land life soul earth steel
    destiny wind fire roots rain moon tide dusk frost thunder light wave dust ice flame
    seed wing mind storm silence echo peak valley blood sweat tear laughter clay iron
    gold silver bronze fog heat leaf branch trunk bark gear wheel lever spring cog code
    bit byte circuit data ghost spirit vision memory wolf bear eagle fish deer hawk owl
    fox rabbit snake current anchor sail rudder map compass sextant rope knot hammer
    anvil forge tong bellow plow scythe hoe barn pen ink paper book scroll clock hand
    bell mirror lens prism glass frame thread needle loom dye stitch pot pan oven spice
    plate road bridge tunnel gate wall song drum string horn flute paint brush canvas
    color dice card board pawn king lock key chain bar door storm wind rain sun ice
    snow hail mist dew grave flower dirt hill cliff cave plain wood lake pond stream
    fall spring ship oar net hook deck planet comet void sleep nightmare rest war peace
    sword shield flag love hate grief trust fear pride shame rage calm book school
    chalk lesson mind hut tent roof floor name title word deed legend coin gem trade
    scale market crow raven dove vulture sparrow steam smoke ash glow vine moss fern
    thorn petal web silk spider fly ant quartz coal salt sand ghost fate life soul
    breeze gust squall gale blizzard fog drizzle shower torrent flood ripple puddle
    brook creek delta ridge slope mesa bluff dune crater lava magma ash ember spark
    flare torch lantern beacon candle wick oil wax quilt blanket rug mat cloak cape
    hood scarf glove boot sandal heel sole lace buckle strap pin nail screw bolt rivet
    hinge latch clasp hook eye loop ring band crown tiara helm visor mask lens scope
    sight glare beam ray pulse beat rhythm tone pitch chord melody echo clang crash
    thud boom roar growl purr hiss snap twig branch stump log plank board beam rafter
    joist truss ridge peak summit base ledge shelf nook cranny gap chasm rift fault
    seam vein ore gem crystal shard flake dust grain pebble boulder slab tile brick
    mortar clay silt loam turf sod grass weed bush shrub vine ivy moss lichen fungus
    spore root stem bud bloom fruit seed pod husk shell rind peel pith core flesh bone
    sinew muscle nerve vein artery lung breath throat tongue lip jaw chin brow lash lid
    pupil iris gleam twinkle flash shimmer sheen gloss polish rust tarnish patina dent
    scratch nick gouge crack split rift tear hole pit trench ditch moat rampart tower
    spire dome arch vault crypt tomb coffin))

;; --- Qubit Operations (Imported from Cht4_Qu_Tra_Hyb_Up_V2rev3_revisited_4_23_25.lisp) ---
(defstruct qubit alpha beta measured-p)

(defun make-qubit (&key (alpha 1.0) (beta 0.0) (measured-p nil))
  "Creates a qubit with given alpha, beta, and measured-p status."
  (unless (numberp alpha)
    (error "Alpha must be a number, got: ~a" alpha))
  (unless (numberp beta)
    (error "Beta must be a number, got: ~a" beta))
  (let ((norm (sqrt (+ (* (abs alpha) (abs alpha)) (* (abs beta) (abs beta))))))
    (if (zerop norm)
        (error "Alpha and beta cannot both be zero")
        (let ((a (/ alpha norm))
              (b (/ beta norm)))
          (make-qubit :alpha a :beta b :measured-p measured-p)))))

(defun normalize-qubit (q)
  "Ensures |alpha|^2 + |beta|^2 = 1, handling complex numbers."
  (let* ((a (qubit-alpha q))
         (b (qubit-beta q))
         (norm (sqrt (+ (* (abs a) (abs a)) (* (abs b) (abs b)))))
         (a* (/ a norm))
         (b* (/ b norm)))
    (make-qubit :alpha a* :beta b* :measured-p (qubit-measured-p q))))

(defun hadamard (q)
  (if (qubit-measured-p q)
      (error "Cannot apply gate to a measured qubit: ~a" q)
      (let* ((a (qubit-alpha q))
             (b (qubit-beta q))
             (new-a (/ (+ a b) (sqrt 2)))
             (new-b (/ (- a b) (sqrt 2))))
        (normalize-qubit (make-qubit :alpha new-a :beta new-b :measured-p nil)))))

(defun measure (q)
  (if (qubit-measured-p q)
      q
      (let ((p0 (expt (abs (qubit-alpha q)) 2)))
        (if (< (random 1.0) p0)
            (make-qubit :alpha 1.0 :beta 0.0 :measured-p t)
            (make-qubit :alpha 0.0 :beta 1.0 :measured-p t)))))

;; --- Utility Functions ---

(defun split-string (str)
  "Splits a string into a list of words using space as the delimiter."
  (loop for start = 0 then (1+ end)
        for end = (position #\Space str :start start)
        for word = (subseq str start end)
        while (and word (> (length word) 0))
        collect word
        while end
        do (setf start end)))

(defun flatten (lst)
  "Flattens nested lists."
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))

(defun escape-string (s)
  "Escapes double quotes for DOT labels."
  (substitute #\\ #\" (princ-to-string s)))

(defun memory-node-strength (node)
  "Placeholder for node strength."
  (if node 1.0 0.0))

(defun memory-node-content (node)
  "Placeholder for node content."
  (or node "unknown"))

(defun find-strongest-node ()
  "Finds node with highest strength."
  (let ((strongest-id nil)
        (max-strength -1.0))
    (maphash (lambda (id node)
               (when (> (memory-node-strength node) max-strength)
                 (setf max-strength (memory-node-strength node))
                 (setf strongest-id id)))
             *pete-memory-graph*)
    (or strongest-id "treehouse")))

(defun random-memory-walk (start-id steps)
  "Walks memory graph randomly for STEPS from START-ID."
  (let ((trail (list start-id)))
    (loop repeat (1- steps)
          for keys = (alexandria:hash-table-keys *pete-memory-graph*)
          when keys
          do (push (nth (random (length keys)) keys) trail))
    trail))
    
    
(defun safe-memory-entry (entry)
  "Safely converts Pete's memory entries to strings for output."
  (cond
    ((stringp entry) entry)
    ((symbolp entry) (symbol-name entry))
    ((and (listp entry) (every #'stringp entry))
     (format nil "~{~a~^ ~}" entry))
    ((and (listp entry) (every #'symbolp entry))
     (format nil "~{~a~^ ~}" (mapcar #'symbol-name entry)))
    (t "UNKNOWN-THOUGHT")))


;; --- Core Functions ---
(defun if++ (stuff &optional (holes 0))
  "Advanced probabilistic routine driven by memory and holes, with transistor-like precision."
  (let* ((wild-factor (min 5 (if (> holes 0)
                                 (+ 2 (random (max 4 (floor holes 2))))
                                 (1+ (random 3)))))
         (flat-stuff (if (listp stuff) (flatten stuff) (list stuff)))
         (bias (if (and flat-stuff (member (car flat-stuff) *nouns*)) 1 0))
         (adjusted-wild-factor (+ bias wild-factor))
         (start-id (cond
                     ((symbolp (car flat-stuff)) (string-downcase (symbol-name (car flat-stuff))))
                     ((listp (car flat-stuff)) (string-downcase (symbol-name (caar flat-stuff))))
                     (t nil)))
         (fallback-id "treehouse")
         (root-id (or start-id (find-strongest-node) fallback-id))
         (root-node (gethash root-id *pete-memory-graph*)))
    (format t "Wild factor boosted: ~A (holes: ~A, bias: ~A)~%" adjusted-wild-factor holes bias)
    (unless root-node
      (format t "Root node '~a' not found. Defaulting to 'treehouse'.~%" root-id)
      (setf root-node (gethash fallback-id *pete-memory-graph*)))
    (let ((thought-trail (random-memory-walk root-id adjusted-wild-factor)))
      (format t "~%🧠 Thought Trail from ~a: ~a~%" root-id thought-trail)
      (let* ((muddy-thoughts (if (> holes 4)
                                 (append thought-trail '(mud))
                                 thought-trail)))
        (format t "🌀 Final Thoughts: ~a~%" muddy-thoughts)
        muddy-thoughts))))

(defun twist-back (stuff)
  "Twists input back based on wild factor, ensuring a list output."
  (let ((result (if stuff
                    (if (listp stuff)
                        (mapcar #'string-upcase stuff)
                        (list (string-upcase stuff)))
                    '(TWIST-BACK-DEFAULT))))
    (format t "Twist-back result: ~a~%" result)
    result))
    
    
(defun vibe-check (stuff)
  "Checks the vibe of the input, ensuring a list output."
  (let ((result (cond
                  ((null stuff) '(VIBE-CHECK-DEFAULT))
                  ((listp stuff)
                   (remove-if (lambda (x) (member x '(MUD NIL))) stuff))
                  (t (list stuff)))))
    (format t "Vibe-check result: ~a~%" result)
    (if (null result) '(VIBE-CHECK-FALLBACK) result)))

(defun visualize-holes-matrix (holes words)
  "Shows holes as dots in a 5x5 matrix."
  (format t "~%=== Holes Matrix ===~%")
  (format t "Holes: ~a | Words: ~a~%" holes words)
  (let ((grid-size 5)
        (word-len (length words)))
    (dotimes (y grid-size)
      (dotimes (x grid-size)
        (if (and (< x word-len) (not (member (nth x words) (append *verbs* *nouns*))))
            (format t ". ")
            (format t "  ")))
      (format t "~%"))
    (format t "Dots mark holes in convo space!~%"))
  (format t "=================~%"))

(defun split-and-tag (input)
  "Splits and tags input into verbs, nouns, or other."
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
      (visualize-holes-matrix holes untagged))
    (values tagged holes)))
       
(defun quantum-mode-switch (mode holes base-holes q)
  (let ((alpha-prob (expt (abs (qubit-alpha q)) 2))
        (beta-prob (expt (abs (qubit-beta q)) 2)))
    (ecase mode
      (:transistor
       (let ((gain (1+ (random 3))))
         (format t "Transistor mode: gain ~a modulated by alpha probability ~,2f~%" gain alpha-prob)
         (min (round (* gain holes (+ 1 alpha-prob))) 10)))
      (:superposition
       (let ((synth (random (+ base-holes holes))))
         (format t "Superposition mode: synth ~a modulated by beta probability ~,2f~%" synth beta-prob)
         (min 10 (round (+ holes synth beta-prob)))))
      (:tunneling
       (let ((leak (random holes)))
         (format t "Tunneling mode: leak ~a modulated by alpha probability ~,2f~%" leak alpha-prob)
         (max 1 (round (- holes (* leak alpha-prob)))))))))

;; Groks Quantum State to Transistor Holes April 28th 2025
;; Accomodating a more sophisticated uproach or representation to the Qubit Facets In Binary CPU's
;; Requested for an alternative uproach lookign toforward the Quantum Computing Logic

;; Groks Quantum State to Transistor Holes April 28th 2025
;; Accomodating a more sophisticated uproach or representation to the Qubit Facets In Binary CPU's
;; Requested for an alternative uproach lookign toforward the Quantum Computing Logic

(defun quantum-hole-breathe (holes &optional current-input mode alpha beta)
  "Breathes quantum holes into the conversation flow, with quantum states influencing holes."
  (handler-case
      (sb-ext:with-timeout 8  ; Timeout after 8 seconds
        (let* ((current-symbols (if current-input
                                    (remove-if-not #'symbolp current-input)
                                    '(DEFAULT INPUT)))
               (past-memories (sort
                               (remove-if-not
                                (lambda (mem)
                                  (and (listp mem)
                                       (intersection current-symbols mem :test #'equal)))
                                *pete-memory*)
                               #'>
                               :key (lambda (mem)
                                      (length (intersection current-symbols mem :test #'equal)))))
               (past (if past-memories
                         (first past-memories)
                         (nth (random (length *pete-memory*)) *pete-memory*)))
               (past2 (if (and past-memories (> (length past-memories) 1))
                          (second past-memories)
                          (nth (random (length *pete-memory*)) *pete-memory*)))
               (merged (append (remove 'mud past) (remove 'mud past2)))
               (filtered (remove-if-not #'symbolp merged))
               (base-holes (count-if-not (lambda (x)
                                           (or (member x *verbs*) (member x *nouns*)))
                                         filtered))
               (overlap (length (intersection (remove-if-not #'symbolp past)
                                              (remove-if-not #'symbolp past2)
                                              :test #'equal)))
               (q (hadamard (make-qubit :alpha (or alpha 0.7) :beta (or beta 0.7))))
               (q (measure q))
               (hole-adjustment (round (* 5 (- (qubit-beta q) (qubit-alpha q)))))
               (adjusted-holes (max 1 (+ holes hole-adjustment)))
               (quantum-mode (or mode
                                 (if (eq *current-mode* :auto)
                                     (cond ((>= (qubit-alpha q) 0.9) :transistor)
                                           ((>= (qubit-beta q) 0.9) :superposition)
                                           (t :tunneling))
                                     (cond
                                       ((>= adjusted-holes 4) :superposition)
                                       ((and (> overlap 1) (< adjusted-holes 4)) :tunneling)
                                       (t :transistor)))))
               (final-holes (quantum-mode-switch quantum-mode (max 1 adjusted-holes) base-holes q))
               (twist-options '(RISE FALL SPIKE CREEP GLOW SOIL SEED))
               (twist (or (intersection filtered twist-options :test #'equal)
                          (list (nth (random (length twist-options)) twist-options))))
               (wild (if++ twist final-holes))
               (id (random 1000))
               (tagged (list 'QUANTUM-ADAPTED wild id q quantum-mode)))
          (format t "Quantum mode ~a | Adjusted Holes: ~a (base: ~a, adjustment: ~a, final: ~a, overlap: ~a)~%"
                  quantum-mode adjusted-holes base-holes hole-adjustment final-holes overlap)
          (format t "Quantum adaptation [#~a]: twist ~a + ~a → ~a~%"
                  id past past2 tagged)
          tagged))
    (error (e)
      (format t "Error in quantum-hole-breathe: ~a~%" e)
      '(FALLBACK-TWIST (MUD)))
    (sb-ext:timeout (e)
      (format t "Timeout in quantum-hole-breathe: ~a~%" e)
      '(FALLBACK-TWIST (MUD)))))


      
(defun quantum-induce (&optional (alpha 0.7) (beta 0.7))
    "Induces quantum effect with given alpha and beta, logging adjusted holes."
    (handler-case
        (sb-ext:with-timeout 5
          (let* ((q (hadamard (make-qubit :alpha alpha :beta beta)))
                 (q-measured (measure q))
                 (holes (random 5))
                 (twisted (quantum-hole-breathe holes nil nil (qubit-alpha q-measured) (qubit-beta q-measured)))
                 (adjusted-holes (if (eq (first twisted) 'QUANTUM-ADAPTED)
                                     (second twisted)  ; Extract 'wild' which is influenced by final-holes
                                     holes)))
            (format t "Quantum induction: alpha=~a, beta=~a, initial holes=~a, adjusted holes=~a -> ~a~%"
                    alpha beta holes adjusted-holes twisted)
            (push twisted *pete-memory*)
            (when (> (length *pete-memory*) *memory-cap*)
              (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
            twisted))
      (error (e) (format t "Error in quantum-induce: ~a~%" e) '(QUANTUM-INDUCE-FAILED))
      (sb-ext:timeout (e) (format t "Timeout in quantum-induce: ~a~%" e) '(QUANTUM-INDUCE-TIMEOUT))))
      
      

      
      

(defun test-quantum-switch ()
  "Test output of quantum-mode-switch with each mode."
  (let ((holes 4)
        (base-holes 3)
        (q (hadamard (make-qubit :alpha 1.0 :beta 0.0))))
    (format t "~%--- Testing Quantum Mode Switch ---~%")
    (format t "TRANSISTOR:     ~a~%" (quantum-mode-switch :transistor holes base-holes q))
    (format t "TUNNELING:      ~a~%" (quantum-mode-switch :tunneling holes base-holes q))
    (format t "SUPERPOSITION:  ~a~%" (quantum-mode-switch :superposition holes base-holes q))))

(defun synaptic-treaty-upgrade (quad-matrix)
  "Unifies quadrants into a treaty node."
  (let* ((heard (getf quad-matrix :heard))
         (recalled (getf quad-matrix :recalled))
         (reacted (getf quad-matrix :reacted))
         (spoke (getf quad-matrix :spoke))
         (resonant-core (intersection heard recalled :test #'equal))
         (twist-core (intersection reacted spoke :test #'equal))
         (binding (union resonant-core twist-core))
         (signature (list 'TREATY-NODE binding (random 1000))))
    (when binding
      (format t "~%🌌 Treaty Achieved: Resonant core binding established.~%")
      (push signature *pete-memory*)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      signature)))
      
;; --- Core Functions ---


(defun find-resonance (nodes)
  "Stubs resonance finding."
  nodes)

(defun synthesize-response (resonant)
  "Stubs response synthesis."
  (if resonant (car resonant) '(NO-RESPONSE)))

(defun commit-to-finality (synth)
  "Stubs finality commit."
  (format t "Final treaty response: ~a~%" synth)
  synth)
  
(defun activate-nodes (node-tree input)
  "Stubs node activation."
  (declare (ignore node-tree))
  (list input))
  

(defun finalize-treaty-response (node-tree input)
  "Finalizes treaty response."
  (let* ((activated (activate-nodes node-tree input))
         (resonant (mapcar #'find-resonance activated))
         (synth (synthesize-response resonant)))
    (commit-to-finality synth)))
       

  
(defstruct treaty-node
  id symbols vibe holes-range quantum-tag memory-links quadrant-activation effect)

(defun create-treaty (id effect symbols &optional source-memory vibe)
  "Creates a treaty and stores it."
  (declare (ignore source-memory))
  (let ((treaty (make-treaty-node
                 :id id
                 :effect effect
                 :symbols symbols
                 :vibe (or vibe '(neutral 0))
                 :memory-links nil
                 :holes-range '(0 5)
                 :quantum-tag (random 1000)
                 :quadrant-activation '(heard recalled))))
    (setf (gethash id *pete-treaty-map*) treaty)
    (format t "Treaty '~a' created and stored.~%" id)
    treaty))

(defun get-treaty (id)
  "Retrieves a treaty by ID."
  (gethash id *pete-treaty-map*))

(defun link-treaties (id1 id2)
  "Links two treaties bidirectionally."
  (let ((t1 (get-treaty id1))
        (t2 (get-treaty id2)))
    (when (and t1 t2)
      (push id2 (treaty-node-memory-links t1))
      (push id1 (treaty-node-memory-links t2))
      (setf (gethash id1 *pete-treaty-map*) t1)
      (setf (gethash id2 *pete-treaty-map*) t2)
      (format t "Linked ~a <-> ~a~%" id1 id2))))

(defun generate-treaty-digraph (treaty-ids output-file)
  "Generates a Graphviz DOT file for treaty network."
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (format stream "digraph PeteTreatyMap {~%")
    (format stream "  node [shape=box style=filled fillcolor=lightgoldenrod];~%")
    (dolist (id treaty-ids)
      (let ((treaty (get-treaty id)))
        (when treaty
          (let ((label (format nil "~a\\nVibe: ~a\\nSymbols: ~a"
                               (escape-string (treaty-node-effect treaty))
                               (treaty-node-vibe treaty)
                               (treaty-node-symbols treaty))))
            (format stream "  \"~a\" [label=\"~a\" fillcolor=\"~a\"];~%"
                    id label
                    (case (car (treaty-node-vibe treaty))
                      (steady "lightblue")
                      (bright "yellow")
                      (neutral "white")
                      (t "lightgoldenrod")))))))
    (dolist (id treaty-ids)
      (let ((treaty (get-treaty id)))
        (when treaty
          (dolist (link (treaty-node-memory-links treaty))
            (format stream "  \"~a\" -> \"~a\" [style=dashed];~%" id link)))))
    (format stream "}~%"))
  (format t "Treaty network exported to ~a~%" output-file))

(defun render-treaty-digraph (dot-file output-file)
  "Renders a DOT file to PNG using Graphviz."
  (handler-case
      (progn
        (uiop:run-program (format nil "dot -Tpng ~a -o ~a" dot-file output-file)
                          :output t :error-output t)
        (format t "Graph rendered to ~a~%" output-file))
    (error (e)
      (format t "Error rendering treaty digraph: ~a~%" e))))

(defun generate-neural-diagram (output-file)
  "Generates a DOT file for memory entries and hole connections, now with qubit info."
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (format stream "digraph NeuralFlow {~%")
    (format stream "  node [shape=circle style=filled fillcolor=lightblue];~%")
    (loop for entry in *pete-memory*
          for idx from 1
          do (let ((holes (if (listp entry)
                              (count-if-not (lambda (x)
                                              (or (member x *verbs*) (member x *nouns*)))
                                            entry)
                              0))
                   (q (if (and (listp entry) (eq (car entry) 'QUANTUM-ADAPTED))
                          (fourth entry)
                          nil))
                   (mode (if (and (listp entry) (eq (car entry) 'QUANTUM-ADAPTED))
                             (fifth entry)
                             nil)))
               (format stream "  \"m~a\" [label=\"~a\\nHoles: ~a~a\" fillcolor=\"~a\"];~%"
                       idx entry holes
                       (if q (format nil "\\nMode: ~a\\nQubit: alpha=~a beta=~a"
                                     mode (qubit-alpha q) (qubit-beta q))
                           "")
                       (if (> holes 3) "pink" "lightblue"))))
    (loop for idx from 1 below (length *pete-memory*)
          do (format stream "  \"m~a\" -> \"m~a\" [label=\"flow\"];~%" idx (1+ idx)))
    (loop for entry in *pete-memory*
          for idx from 1
          when (and (listp entry) (eq (car entry) 'QUANTUM-ADAPTED))
          do (format stream "  \"m~a\" -> \"q~a\" [label=\"quantum\" style=dashed];~%"
                     idx (caddr entry))
             (format stream "  \"q~a\" [label=\"Quantum ~a\" shape=diamond fillcolor=yellow];~%"
                     (caddr entry) (caddr entry)))
    (format stream "}~%"))
  (format t "Neural diagram exported to ~a~%" output-file))
  
  
(defun render-neural-diagram (dot-file output-file)
  "Renders neural DOT to PNG."
  (handler-case
      (progn
        (uiop:run-program (format nil "dot -Tpng ~a -o ~a" dot-file output-file)
                          :output t :error-output t)
        (format t "Neural diagram rendered to ~a~%" output-file))
    (error (e)
      (format t "Error rendering neural diagram: ~a~%Ensure Graphviz is installed and 'dot' is in your PATH.~%" e))))

(defun treaty-scan (input)
  "Scans input symbols against treaties."
  (let ((matches '()))
    (maphash
     (lambda (id node)
       (let ((sym-match (intersection input (treaty-node-symbols node) :test #'equal)))
         (when sym-match
           (push (list :id id
                       :match sym-match
                       :effect (treaty-node-effect node)
                       :vibe (treaty-node-vibe node)
                       :links (treaty-node-memory-links node))
                 matches))))
     *pete-treaty-map*)
    (if matches
        (progn
          (format t "~%🧭 Treaty Matches Found:~%")
          (dolist (m matches)
            (format t "• ~a → ~a~%   Symbols: ~a~%   Vibe: ~a~%   Linked to: ~a~%"
                    (getf m :id)
                    (getf m :effect)
                    (getf m :match)
                    (getf m :vibe)
                    (getf m :links)))
          matches)
        (format t "🚫 No treaty match found.~%"))))

;; --- Export Memory ---
(defun export-memory-log (&optional (filename "pete_AI_beast_memory.txt"))
  "Exports memory to file."
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry *pete-memory*)
      (format stream "~a~%" entry)))
  (format t "Memory exported to ~a~%" filename))

;; --- Initialize Treaties ---
(create-treaty 'truth-light "Truth bends light into meaning" '(TRUTH BENDS LIGHT) nil '(steady 333))
(create-treaty 'mudlight-accord "Sun dries tears over mud" '(MUD SUN DRIES TEARS) nil '(bright 523))
(link-treaties 'truth-light 'mudlight-accord)

(defun pete-flow (stuff &optional (depth 0) (holes 0))
  "Processes input through Pete’s flow with quantum adaptation."
  (if (>= depth *pete-depth-limit*)
      (progn
        (format t "Pete’s done—memory: ~a~%" *pete-memory*)
        stuff)
      (progn
        (push (if (listp stuff) stuff (list stuff)) *pete-memory*)
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        (format t "Pete, depth ~a, got: ~a (mem size: ~a, holes: ~a)~%"
                depth stuff (length *pete-memory*) holes)
        (let* ((wild (if++ stuff holes))
               (twist (vibe-check (twist-back wild)))
               (quantum-twist (quantum-hole-breathe holes twist)))  ; Add quantum effect
          (if (member 'quit twist)
              (progn
                (format t "Memory: ~a~%Later, Pete!~%" *pete-memory*)
                nil)
              (progn
                (format t "Flowing on: ~a~%" quantum-twist)
                (pete-flow quantum-twist (1+ depth) holes)))))))
                
                
(defun pete-install-memory (entry)
  "Installs a memory entry."
  (push entry *pete-memory*)
  (when (> (length *pete-memory*) *memory-cap*)
    (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
  (format t "Installed entry into Pete's memory: ~a~%" entry))

(defun pete-listen (input)
  "Listens to input, updates memory with quantum adaptation."
  (format t "Pete listens: ~a~%" input)
  (let* ((split (multiple-value-list (split-and-tag input)))
         (tagged (car split))
         (holes (cadr split))
         (thought (pete-flow (mapcar #'cadr tagged) 0 holes))  ; Extract symbols from tagged list
         (twisted (quantum-hole-breathe holes thought)))
    (format t "Pete heard: ~a~%" thought)
    (push twisted *pete-memory*)
    (when (> (length *pete-memory*) *memory-cap*)
      (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
    thought))
    
(defun pete-react (input)
  "Reacts to input, updates memory with quantum adaptation."
  (format t "Pete reacts: ~a~%" input)
  (let* ((split (multiple-value-list (split-and-tag input)))
         (tagged (car split))
         (holes (cadr split))
         (thought (pete-flow (mapcar #'cadr tagged) 0 holes))  ; Extract symbols from tagged list
         (twisted (quantum-hole-breathe holes thought)))
    (format t "Pete reacts: ~a~%" thought)
    (push twisted *pete-memory*)
    (when (> (length *pete-memory*) *memory-cap*)
      (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
    thought))

(defun pete-reacts (flow-result)
    "Reacts to the conversation flow result, handling both lists and tagged results."
    (let ((react-str (cond
                       ((and (listp flow-result) (every #'stringp flow-result))
                        (format nil "~{~a~^ ~}" flow-result))
                       ((listp flow-result)
                        (format nil "~a" flow-result))
                       (t (format nil "~a" flow-result)))))
      (format t "Pete reacts: ~a~%" react-str)
      react-str))
       
(defun pete-read ()
  "Recalls a random memory safely."
  (if *pete-memory*
      (let ((memory-pick (nth (random (length *pete-memory*)) *pete-memory*)))
        (format t "Pete reads memory: ~a~%" (safe-memory-entry memory-pick))
        (let ((twist (twist-back (safe-memory-entry memory-pick))))
          (format t "Pete recalls: ~a~%" twist)
          twist))
      (progn
        (format t "Pete’s memory is empty—nothing to read!~%")
        '(PETE HUMS))))



(defun local-knowledge ()
  "Shares a knowledge snippet."
  (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
    (format t "Pete knows: ~a~%" snippet)
    (multiple-value-bind (untagged holes) (split-and-tag snippet)
      (let ((twisted (pete-flow untagged 0 holes)))
        (push twisted *pete-memory*)
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        twisted))))

(defun pete-speak ()
  "Speaks freely from memory and knowledge, ensuring quantum adaptation."
  (let* ((local-pick (if *pete-memory*
                         (nth (random (length *pete-memory*)) *pete-memory*)
                         '(PETE VIBES)))
         (know-pick (if *local-knowledge*
                        (nth (random (length *local-knowledge*)) *local-knowledge*)
                        '(KNOWLEDGE MISSING)))
         (know-twist (multiple-value-bind (tagged holes)
                         (split-and-tag know-pick)
                       (pete-flow (mapcar #'cadr tagged) 0 holes)))
         (combined (if (or (null local-pick) (eq local-pick 'NIL))
                       know-twist
                       (append local-pick know-twist)))
         (holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              (if (listp combined) combined '(FALLBACK))))
         (twisted (quantum-hole-breathe holes (if (listp combined) combined '(FALLBACK)))))
    (format t "Pete speaks free: ~a~%" twisted)
    (push twisted *pete-memory*)  ; twisted is already a list
    (when (> (length *pete-memory*) *memory-cap*)
      (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
    twisted))

(defun pete-think ()
  "Generates a thought from memory."
  (when *pete-memory*
    (let* ((past (nth (random (length *pete-memory*)) *pete-memory*))
           (holes (count-if-not (lambda (x)
                                  (or (member x *verbs*) (member x *nouns*)))
                                past))
           (boosted-holes (min (+ holes (random 5)) 10)))
      (let ((twist (if++ (twist-back past) boosted-holes)))
        (format t "Pete thinks: Old ~a twists to ~a (holes: ~a)~%" past twist boosted-holes)
        (push twist *pete-memory*)
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        twist))))

(defun pete-reason (input)
  (format t "~%Pete reasons...~%")
  (let* ((tagged (split-and-tag input))
         (thought (pete-flow (car (multiple-value-list tagged)) 0
                             (cadr (multiple-value-list tagged)))))
    (format t "Thought: ~a~%" thought)
    thought))

(defun pete-learn (input)
  "Updates knowledge from input."
  (let* ((tagged (split-and-tag input))
         (untagged (car (multiple-value-list tagged)))
         (new-knowledge (format nil "~a ~a ~a"
                                (or (nth 0 untagged) "thing")
                                (or (nth 1 untagged) "does")
                                (or (nth 2 untagged) "stuff"))))
    (format t "Pete learns: Adding '~a' to knowledge~%" new-knowledge)
    (push new-knowledge *local-knowledge*)
    (pete-install-memory untagged)
    new-knowledge))

(defun pete-act (input)
  "Generates robot actions."
  (let* ((tagged (split-and-tag input))
         (untagged (car (multiple-value-list tagged)))
         (action (cond
                   ((member 'MOVE untagged) '(ROBOT MOVE FORWARD))
                   ((member 'SPEAK untagged) '(ROBOT SAY "Pete’s alive!"))
                   (t '(ROBOT THINK DEEP))))
         (new-memory (list 'ACTION action)))
    (format t "Pete acts: Command ~a~%" action)
    (pete-install-memory new-memory)
    action))
    


;; --- Visualization and Telemetry ---
(defun echo-memory ()
  "Prints current memory."
  (format t "~%=== Pete's Memory Echo ===~%")
  (loop for entry in *pete-memory*
        for idx from 1 do
        (format t "~a. ~a~%" idx entry))
  (format t "~%===========================~%"))

(defun pete-visualize (heard recalled reacted spoken)
  "Displays quad matrix for convo."
  (format t "~%=== Pete’s Ultimate Quad Matrix (Convo 1) ===~%")
  (format t ">>> Quadrant 1: Heard (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))

(defun pete-visualize-add (heard recalled reacted spoken)
  "Displays quad matrix for second convo."
  (format t "~%=== Pete’s Additions Quad Matrix (Convo 2) ===~%")
  (format t ">>> Quadrant 1: Heard 2 (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled 2 (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted 2 (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke 2 (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))

(defun pete-telemetry ()
  "Shows memory stats."
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
  "Handles dual conversations."
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
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      (export-memory-log)
      (list spoken1 spoken2))))  



;; --- Mirror and Scintillation ---
(defun push-to-mirror (result mirror-file)
  "Writes result to a file."
  (with-open-file (stream mirror-file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~S~%" result))
  (format t "Pushed to mirror: ~A~%" mirror-file))

(defun read-mirror (mirror-file)
  "Reads results from mirror file."
  (with-open-file (stream mirror-file
                          :direction :input
                          :if-does-not-exist :error)
    (loop for line = (read stream nil nil)
          while line
          collect line)))

(defun test-mirror ()
  "Tests mirror push."
  (let ((data '(WILLING TO WORK THROUGH QUEST))
        (mirror "mirror-data.txt"))
    (push-to-mirror data mirror)))

(defun sim-scintillation (energy)
  "Simulates scintillator pulses."
  (let ((photons (round (* energy 40))))
    (format t "Energy: ~A keV -> ~A photons~%" energy photons)
    photons))

(defun mirror-to-file (photons mirror-file)
  "Mirrors scintillation pulses."
  (with-open-file (stream mirror-file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "(SCINTILLATION ~A)~%" photons))
  (format t "Mirrored to ~A~%" mirror-file))

(defun test-scint-mirror ()
  "Tests scintillation mirror."
  (let ((energy 10)
        (mirror "scint-mirror.txt"))
    (mirror-to-file (sim-scintillation energy) mirror)))
    
    
(defun converse-no-reset ()
  "Conversation loop without resetting memory."
  (format t "~%=== Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'reason', 'learn', 'act', 'treaty-scan', 'neural-diagram', 'quantum-induce [alpha beta]') ===~%")
  (finish-output)
  (handler-case
      (loop
        (format t "> ")
        (finish-output)
        (let* ((input (string-downcase (string-trim " " (read-line))))
               (input-parts (split-string input)))
          (cond ((string= input "quit")
                 (format t "Pete sums it: Twisted NIL into wild vibes!~%")
                 (format t "Pete waves: See ya! (say 'jump-back' to dive in again)~%")
                 (finish-output)
                 (let ((next (string-trim " " (read-line))))
                   (when (string= next "jump-back")
                     (jump-back)))
                 (return))
                ((string= input "know") (local-knowledge))
                ((string= input "speak") (pete-speak))
                ((string= input "listen")
                 (format t "Enter input to listen: ")
                 (finish-output)
                 (pete-listen (read-line)))
                ((string= input "read") (pete-read))
                ((string= input "react")
                 (format t "Enter input to react: ")
                 (finish-output)
                 (pete-react (read-line)))
                ((string= input "communicate")
                 (format t "Enter first convo: ")
                 (finish-output)
                 (let ((input1 (read-line)))
                   (format t "Enter second convo: ")
                   (finish-output)
                   (let ((input2 (read-line)))
                     (pete-communicate input1 input2))))
                ((string= input "export") (export-memory-log))
                ((string= input "reason")
                 (format t "Enter input to reason: ")
                 (finish-output)
                 (pete-reason (read-line)))
                ((string= input "learn")
                 (format t "Enter input to learn: ")
                 (finish-output)
                 (pete-learn (read-line)))
                ((string= input "act")
                 (format t "Enter input to act: ")
                 (finish-output)
                 (pete-act (read-line)))
                ((string= input "treaty-scan")
                 (format t "Enter symbols (space-separated): ")
                 (finish-output)
                 (let* ((symbols (mapcar #'intern (split-string (read-line))))
                        (result (treaty-scan symbols)))
                   (format t "Scan result: ~a~%" result)))
                ((string= input "neural-diagram")
                 (generate-neural-diagram "neural-flow.dot")
                 (render-neural-diagram "neural-flow.dot" "neural-flow.png"))
                ((string= (first input-parts) "quantum-induce")
                 (let ((alpha (if (> (length input-parts) 1)
                                  (float (parse-number:parse-number (second input-parts)))
                                  0.7))
                       (beta (if (> (length input-parts) 2)
                                 (float (parse-number:parse-number (third input-parts)))
                                 0.7)))
                   (quantum-induce alpha beta)))
                ((string= input "quantum")  ; Add fallback for "quantum"
                 (format t "Applying quantum-induce with default alpha=0.7, beta=0.7~%")
                 (quantum-induce 0.7 0.7))
                ((string= input "jump-back") (jump-back))
                (t
                 (multiple-value-bind (tagged holes) (split-and-tag input)
                   (format t "~%")
                   (let ((result (pete-flow (mapcar #'cadr tagged) 0 holes)))
                     (format t "~%Pete: ~a~%" result)))))))
    (error (e)
      (format t "Caught an error: ~a~%Continuing conversation loop...~%" e))))
      
      
(defun jump-back ()
  "Restarts the conversation loop."
  (converse-no-reset))


#+clisp
(defun push-to-socket (result host port)
  "Pushes result to socket."
  (with-open-stream (socket (socket:socket-connect port host))
    (format socket "~S~%" result)
    (force-output socket))
  (format t "Pushed to socket mirror: ~A:~A~%" host port))

#+clisp
(defun mirror-server (port)
  "Receives mirrored results."
  (let ((server (socket:socket-server port)))
    (unwind-protect converse-no-reset
         (loop (with-open-stream (client (socket:socket-accept server))safe
                 (let ((data (read client)))
                   (format t "Mirror got: ~S~%" data)
                   (push-to-mirror data "mirror-data.txt"))))
      (socket:socket-server-close server))))

#+clisp
(defun test-socket ()
  "Tests socket mirror."
  (let ((data '(WILLING TO WORK THROUGH QUEST)))
    (bt:make-thread (lambda () (mirror-server 12345)) :name "mirror-server")
    (sleep 1)
    (push-to-socket data "localhost" 12345)))

;; --- Start PeteAI ---
(converse-no-reset)
