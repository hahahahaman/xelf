(in-package :xelf)

(defgeneric above
  (SELF &OPTIONAL OTHER))

(defgeneric above-center
  (SELF &OPTIONAL OTHER))

(defgeneric add-node
  (BUFFER NODE &OPTIONAL X Y (Z))
  (:documentation "Add the mode NODE to the BUFFER.
Optionally set the location with X,Y,Z."))

(defgeneric aim
  (NODE HEADING)
  (:documentation "Set the NODE's current heading to HEADING."))

(defgeneric aim-at
  (NODE OTHER-NODE)
  (:documentation "Set the NODE's heading to aim at the OTHER-NODE."))

(defgeneric align-to-pixels
  (SELF))

(defgeneric alternate-tap
  (SELF X Y))

(defgeneric location
  (NODE)
  (:documentation "Return as values the X,Y location of NODE."))

(defgeneric backward
  (NODE DISTANCE)
  (:documentation "Move NODE backward DISTANCE units along the aim direction."))

(defgeneric below
  (SELF))

(defgeneric below-center
  (SELF))

(defgeneric bind-any-default-events
  (SELF))

(defgeneric bind-event
    (NODE EVENT METHOD-NAME)
  (:documentation "Bind the EVENT to invoke the method specified in METHOD-NAME, with
NODE as the sole argument to the method.

EVENT is a list of the form:

       (NAME modifiers...)

NAME is either a keyword symbol identifying the keyboard key, or a
string giving the Unicode character to be bound. MODIFIERS is a list
of keywords like :control, :alt, and so on.

Examples:
  
:  (bind-event self '(:up) 'move-up)
:  (bind-event self '(:down) 'move-down)
:  (bind-event self '(:q :control) 'quit)
:  (bind-event self '(:escape :shift) 'menu)

See also `keys.lisp' for the full table of key and modifier symbols."))

(defgeneric bounding-box
  (NODE)
  (:documentation "Return as multiple values the coordinates of the bounding box for
NODE. These are given in the order (TOP LEFT RIGHT BOTTOM)."))

(defgeneric center-point
  (NODE)
  (:documentation "Return as multiple values the coordinates of the NODE's center point."))

(defgeneric change-image
  (NODE IMAGE)
  (:documentation "Set the image of NODE to be IMAGE."))

(defgeneric collide
  (NODE OTHER-NODE)
  (:documentation "This method is invoked when NODE collides with OTHER-NODE."))

(defgeneric colliding-with-p
  (NODE OTHER-NODE)
  (:documentation "Return non-nil if NODE's bounding box touches OTHER-NODE's bounding
box."))

(defgeneric colliding-with-rectangle-p
  (NODE TOP LEFT WIDTH HEIGHT)
  (:documentation "Return non-nil if NODE is colliding with the given rectangle."))

(defgeneric destroy
  (NODE)
  (:documentation "Destroy this NODE and remove it from any buffers."))

(defgeneric direction-to
  (NODE OTHER-NODE)
  (:documentation "Returns the approximate keyword compass direction between NODE and OTHER-NODE."))

(defgeneric distance-between
  (NODE OTHER-NODE)
  (:documentation "Returns the distance between NODE and OTHER-NODE's center points."))

(defgeneric draw
  (NODE)
  (:documentation "Draw the NODE."))

(defgeneric duplicate
  (NODE &REST INITARGS)
  (:documentation "Make a copy of this NODE."))

(defgeneric emptyp
  (NODE))

(defgeneric follow-with-camera
  (BUFFER NODE)
  (:documentation "Arrange for the BUFFER to follow NODE with the camera as it moves."))

(defgeneric forward
  (NODE DISTANCE)
  (:documentation "Move the NODE forward along its current heading for DISTANCE units."))

(defgeneric get-nodes
  (BUFFER)
  (:documentation "Collect all the nodes in the buffer as a list.
Consider using DO-NODES instead."))

(defgeneric glide-window-to
  (BUFFER X Y &OPTIONAL Z)
  (:documentation "Configure window to glide smoothly to the point X,Y."))

(defgeneric glide-window-to-node
  (BUFFER NODE)
  (:documentation "Configure window to glide smoothly to the NODE."))

(defgeneric handle-collision
  (NODE OTHER-NODE)
  (:documentation "Wraparound for collision handling. You shouldn't need to use this
explicitly."))

(defgeneric handle-event
  (NODE EVENT)
  (:documentation "Attempt to find a binding for EVENT in NODE, and execute it if
found."))

(defgeneric handle-point-motion
  (SELF X Y))

(defgeneric contains-node-p
  (BUFFER NODE)
  (:documentation "Return non-nil if BUFFER contains NODE."))

(defgeneric heading-between
  (NODE OTHER-NODE)
  (:documentation "Return the angle (in radians) of the ray from NODE to OTHER-NODE."))

(defgeneric insert 
    (NODE &OPTIONAL X Y Z)
  (:documentation "Add the NODE to the current buffer, optionally at X,Y,Z."))

(defgeneric install-quadtree
  (SELF))

(defgeneric left-of
  (SELF &OPTIONAL OTHER))

(defgeneric left-of-center
  (SELF &OPTIONAL OTHER))

(defgeneric move
  (NODE HEADING DISTANCE)
  (:documentation "Move the NODE toward HEADING by DISTANCE units."))

(defgeneric move-to
  (NODE X Y &OPTIONAL Z)
  (:documentation "Move the NODE to the point X,Y,Z."))

(defgeneric move-toward
  (NODE DIRECTION &OPTIONAL (STEPS))
  (:documentation "Move the node STEPS units in compass direction
DIRECTION."))

(defgeneric move-window
  (BUFFER DX DY &OPTIONAL DZ)
  (:documentation "Move the buffer's window by DX,DY."))

(defgeneric move-window-to
  (BUFFER X Y &OPTIONAL Z)
  (:documentation "Move the buffer's window to X,Y."))

(defgeneric move-window-to-node
  (BUFFER NODE)
  (:documentation "Move the buffer's window to the node NODE."))

(defgeneric paste-here
  (BUFFER)
  (:documentation "Paste nodes from the clipboard here."))

(defgeneric press
  (SELF X Y &OPTIONAL BUTTON))

(defgeneric quadtree-delete
  (NODE &OPTIONAL (TREE)))

(defgeneric quadtree-insert
  (NODE &OPTIONAL (TREE)))

(defgeneric release
  (SELF X Y &OPTIONAL BUTTON))

(defgeneric remove-node
  (BUFFER NODE)
  (:documentation "Remove the object NODE from BUFFER."))

(defgeneric resize
  (NODE WIDTH HEIGHT)
  (:documentation "Resize the NODE to be WIDTH by HEIGHT units."))

(defgeneric resize-to-background-image
  (BUFFER)
  (:documentation "Resize the buffer to fit its background image."))

(defgeneric resize-to-image
  (NODE)
  (:documentation "Resize the NODE to fit its image."))

(defgeneric scale-window
  (SELF &OPTIONAL (WINDOW-SCALE-X) (WINDOW-SCALE-Y)))

(defgeneric scroll-down
  (SELF))

(defgeneric scroll-left
  (SELF))

(defgeneric scroll-right
  (SELF))

(defgeneric scroll-tap
  (SELF X Y))

(defgeneric scroll-up
  (SELF))

(defgeneric snap-window-to-node
  (BUFFER NODE)
  (:documentation "Snap the window to the node NODE."))

(defgeneric start
  (SELF))

(defgeneric step-toward-heading
  (NODE HEADING &OPTIONAL (DISTANCE))
  (:documentation "Return as multiple values the point DISTANCE units
at angle HEADING away from the center of NODE. "))

(defgeneric stop
  (SELF))

(defgeneric stop-following
  (BUFFER)
  (:documentation "Stop all buffer following."))

(defgeneric tap
  (SELF X Y))

(defgeneric touching-point-p
  (NODE X Y)
  (:documentation "Return non-nil if this NODE's bounding box touches
the point X,Y."))

(defgeneric trim
    (BUFFER)
  (:documentation "Trim excess space from the buffer."))

(defgeneric turn-left
  (NODE RADIANS)
  (:documentation "Increase heading by RADIANS."))

(defgeneric turn-right
  (NODE RADIANS)
  (:documentation "Decrease heading by RADIANS."))

(defgeneric unbind-event
  (NODE EVENT MODIFIERS)
  (:documentation "Remove event binding for EVENT from NODE."))

(defgeneric update
  (NODE)
  (:documentation "Update the node's game logic for one frame."))

(defgeneric update-window-glide
  (SELF))

(defgeneric will-obstruct-p
  (NODE PATH-FINDER)
  (:documentation "Returns non-nil when the node NODE will obstruct
PATH-FINDER during path finding."))

(defgeneric window-bounding-box
  (BUFFER)
  (:documentation "Return as multiple values the world-coordinate
bounding box of the buffer's viewing window."))

(defgeneric x
  (NODE)
  (:documentation "Return the current x-coordinate of the NODE."))

(defgeneric y
  (NODE)
  (:documentation "Return the current y-coordinate of the NODE."))

(defgeneric z
  (NODE)
  (:documentation "Return the current z-coordinate of the NODE."))

;;; Category listings


(defparameter *symbol-categories*
'((NODE :disabled)
  (*AFTER-LOAD-PROJECT-HOOK* hooks)
  (*AFTER-STARTUP-HOOK* hooks)
  (*AUTHOR* :disabled)
  (*BACKGROUND-COLOR* :disabled)
(*BOLD* text)
(*BUFFER* :disabled)
(*BUFFERS* :disabled)
(*CACHED-QUADTREE* :disabled)
(*COLORS* :disabled)
(*COPYRIGHT-NOTICE* nil)
(*CURRENT-DIRECTORY* :disabled)
(*DEBUG-ON-ERROR* :disabled)
(*DEFAULT-FRAME-RATE* :disabled)
(*DEFAULT-JOYSTICK-PROFILE* joystick)
(*DEFAULT-QUADTREE-DEPTH* :disabled)
(*DEFAULT-TEXTURE-FILTER* drawing)
(*DIRECTIONS* math)
(*EVENT-HANDLER-FUNCTION* hooks)
(*EVENT-HOOK* hooks)
(*EXECUTABLE* nil)
(*EXPORT-FORMATS* :disabled)
(*FONT* text)
(*FONT-TEXTURE-FILTER* text)
(*FONT-TEXTURE-SCALE* text)
(*FRAME-RATE* nil)
(*FREQUENCY* sound)
(*FULLSCREEN* nil)
(*GENERIC-JOYSTICK-MAPPING* joystick)
(*GL-SCREEN-HEIGHT* :disabled)
(*GL-SCREEN-WIDTH* :disabled)
(*ITALIC* text)
(*JOYSTICK-AXIS-DEAD-ZONE* joystick)
(*JOYSTICK-AXIS-SIZE* joystick)
(*JOYSTICK-BUTTON-SYMBOLS* joystick)
(*JOYSTICK-DEAD-ZONE* joystick)
(*JOYSTICK-DEVICE* joystick)
(*JOYSTICK-DEVICE-NUMBER* joystick)
(*JOYSTICK-MAPPING* joystick)
(*LINUX* :disabled)
(*MESSAGE-FUNCTION* hooks)
(*MESSAGE-HISTORY* hooks)
(*MESSAGE-HOOK-FUNCTIONS* hooks)
(*MESSAGE-LOGGING* hooks)
(*MESSAGE-QUEUE* :disabled)
(*MONOSPACE* text)
(*NEXT-UPDATE-HOOK* hooks)
(*NOMINAL-SCREEN-HEIGHT* drawing)
(*NOMINAL-SCREEN-WIDTH* drawing)
(*OSX* :disabled)
(*OUTPUT-CHANNELS* sound)
(*OUTPUT-CHUNKSIZE* sound)
(*PERSISTENT-VARIABLES* :disabled)
(PERSISTENT-VARIABLES-FILE :disabled)
(*PERSISTENT-VARIABLES-FILE-NAME* :disabled)
(*POINTER-X* mouse)
(*POINTER-Y* mouse)
(*PRELOAD-IMAGES* :disabled)
(*PRELOAD-SAMPLES* :disabled)
(*PROJECT* nil)
(*PROJECT-DIRECTORIES* nil)
(*PROJECT-PATH* nil)
(*QUADTREE* :disabled)
(*QUADTREE-DEPTH* collision-detection)
(*RESIZABLE* nil)
(*RESIZE-HOOK* hooks)
(*RESOURCE-HANDLERS* :disabled)
(*RESOURCES* :disabled)
(*SANS* text)
(*SCALE-OUTPUT-TO-WINDOW* drawing)
(*SCREEN-HEIGHT* drawing)
(*SCREEN-WIDTH* drawing)
(*SERIF* text)
(*UPDATE-FUNCTION* hooks)
(*UPDATES* nil)
(*USE-ANTIALIASED-TEXT* text)
(*USE-SOUND* nil)
(*USE-TEXTURE-BLENDING* drawing)
(*USER-INIT-FILE-NAME* :disabled)
(*USER-JOYSTICK-PROFILE* joystick)
(*USER-KEYBOARD-LAYOUT* :disabled)
(*USER-PROJECTS-DIRECTORY* :disabled)
(*WINDOW-POSITION* drawing)
(*WINDOW-TITLE* drawing)
(*WINDOWS* :disabled)
(*X11-COLOR-DATA* :disabled)
(ABOVE :disabled)
(ABOVE-CENTER :disabled)
(ADD-HOOK hooks)
(ADD-NODE buffers)
(ADD-RESOURCES :disabled)
(ADD-TAG :disabled)
(ADD-TO-LIST hooks)
(AIM movement)
(AIM-AT movement)
(ALIGN-TO-PIXELS drawing)
(PINNED :disabled)
(REMOVE-BLOCK :disabled)
(REMOVE-TAG :disabled)
(ALTERNATE-TAP mouse)
(ANALOG-STICK-HEADING joystick)
(ANALOG-STICK-PRESSED-P joystick)
(ANALOG-STICK-PRESSURE joystick)
(AT-NEXT-UPDATE nil)
(BACKGROUND-COLOR :disabled)
(BACKGROUND-IMAGE :disabled)
(BACKWARD movement)
(BELOW :disabled)
(BELOW-CENTER :disabled)
(BIND-ANY-DEFAULT-EVENTS :events)
(BIND-EVENT events)
(BIND-EVENT-TO-METHOD :disabled)
(BLEND :disabled)
(BOUNDING-BOX collision-detection)
(BOUNDING-BOX-CONTAINS :disabled)
(BUFFER :disabled)
(BUILD-QUADTREE collision-detection)
(BUTTON-TO-SYMBOL joystick)
(CENTER-POINT movement)
(CFLOAT math)
(CHANGE-IMAGE drawing)
(CLEAR-TEXT-IMAGE-CACHE text)
(COLLIDE collision-detection)
(COLLIDING-WITH-BOUNDING-BOX-P collision-detection)
(COLLIDING-WITH-P collision-detection)
(COLLIDING-WITH-RECTANGLE-P collision-detection)
(COLLISION-TYPE :disabled)
(COLOR :disabled)
(COMPOSE buffers)
(COMPOSE-BELOW buffers)
(COMPOSE-BESIDE buffers)
(CREATE-PATH pathfinding)
(CREATE-PROJECT-IMAGE :disabled)
(CURRENT-BUFFER buffers)
(CURRENT-DIRECTORY nil)
(DEFAULT-EVENTS :disabled)
(DEFAULT-PROJECT-DIRECTORIES :disabled)
(DEFINE-BUFFER :disabled)
(DEFINE-RESOURCE :disabled)
(DEFRESOURCE resources)
(DEFUN-MEMO :disabled)
(DEPTH :disabled)
(DESTROY lifecycle)
(DIRECTION-DEGREES movement)
(DIRECTION-HEADING movement)
(FIND-DIRECTION movement)
(DISABLE-KEY-REPEAT keyboard)
(DISTANCE movement)
(DISTANCE-BETWEEN movement)
(DO-NODES buffers)
(DRAW drawing)
(DRAW-BOX drawing)
(DRAW-IMAGE drawing)
(DRAW-LINE drawing)
(DRAW-STRING text)
(DRAW-STRING-BLENDED text)
(DRAW-STRING-SHADED text)
(DRAW-STRING-SOLID text)
(DRAW-TEXTURED-RECTANGLE drawing)
(DRAW-TEXTURED-RECTANGLE-* drawing)
(DUPLICATE nil)
(EMPTYP :disabled)
(ENABLE-KEY-REPEAT keyboard)
(EVENTS :disabled)
(FIND-BOUNDING-BOX movement)
(FIND-HEADING movement)
(FIND-OBJECT :disabled)
(FIND-PATH pathfinding)
(FIND-PATH-WAYPOINTS pathfinding)
(FIND-RESOURCE resources)
(FIND-RESOURCE-OBJECT resources)
(FIND-RESOURCE-PROPERTY resources)
(FIND-TEXT-IMAGE :disabled)
(FIND-TEXTURE drawing)
(FOLLOW-WITH-CAMERA buffers)
(FOLLOWED-OBJECT :disabled)
(FONT-HEIGHT text)
(FONT-TEXT-WIDTH text)
(FONT-WIDTH text)
(FORWARD movement)
(GET-COLOR :disabled)
(GET-NODES buffers)
(GLIDE-FOLLOW :disabled)
(GLIDE-WINDOW-TO buffers)
(GLIDE-WINDOW-TO-NODE buffers)
(HALT-MUSIC sound)
(HALT-SAMPLE sound)
(HANDLE-COLLISION collision-detection)
(HANDLE-EVENT events)
(HANDLE-POINT-MOTION mouse)
(CONTAINS-NODE-P buffers)
(HEADING :disabled)
(HEADING-BETWEEN movement)
(HEADING-DEGREES math)
(HEADING-DIRECTION movement)
(HEADING-TO-CURSOR :disabled)
(HEADING-TO-THING :disabled)
(HEIGHT :disabled)
(HOLDING-CONTROL keyboard)
(HOLDING-SHIFT keyboard)
(IMAGE :disabled)
(IMAGE-HEADING :disabled)
(IMAGE-HEIGHT drawing)
(IMAGE-WIDTH drawing)
(INDEX-ALL-IMAGES resources)
(INDEX-ALL-SAMPLES resources)
(INDEX-PENDING-RESOURCES resources)
(INDEX-RESOURCE resources)
(INITIALIZE :disabled)
(INSERT buffers)
(INSTALL-QUADTREE collision-detection)
(IS-JOYSTICK-EVENT joystick)
(IS-RAW-JOYSTICK-EVENT joystick)
(JOYSTICK-AXIS-PRESSED-P joystick)
(JOYSTICK-AXIS-RAW-VALUE joystick)
(JOYSTICK-AXIS-VALUE joystick)
(JOYSTICK-BUTTON-PRESSED-P joystick)
(JOYSTICK-BUTTON-STATE joystick)
(JOYSTICK-BUTTONS joystick)
(JOYSTICK-EVENT-P joystick)
(JOYSTICK-LEFT-ANALOG-STICK joystick)
(JOYSTICK-NAME joystick)
(JOYSTICK-PROFILE joystick)
(JOYSTICK-RIGHT-ANALOG-STICK joystick)
(JOYSTICK-TYPE joystick)
(KEYBOARD-DOWN-P keyboard)
(KEYBOARD-HELD-P keyboard)
(KEYBOARD-KEYS-DOWN keyboard)
(KEYBOARD-MODIFIER-DOWN-P keyboard)
(KEYBOARD-MODIFIERS keyboard)
(KEYBOARD-PRESSED-P keyboard)
(KEYBOARD-RELEASED-P keyboard)
(KEYBOARD-TIME-IN-CURRENT-STATE keyboard)
(KEYBOARD-TIME-IN-PREVIOUS-STATE keyboard)
(KILL-BUFFER :disabled)
(LAST-X :disabled)
(LAST-Y :disabled)
(LAST-Z :disabled)
(LEFT-ANALOG-STICK-HEADING joystick)
(LEFT-ANALOG-STICK-PRESSED-P joystick)
(LEFT-ANALOG-STICK-PRESSURE joystick)
(LEFT-OF :disabled)
(LEFT-OF-CENTER :disabled)
(LOAD-PROJECT-IMAGE :disabled)
(LOAD-RESOURCE resources)
(LOAD-USER-INIT-FILE :disabled)
(LOAD-VARIABLES :disabled)
(LOCATION movement)
(MAKE-DIRECTORY-MAYBE nil)
(MAKE-EVENT events)
(MAKE-KEYWORD :disabled)
(MAKE-NON-KEYWORD :disabled)
(MESSAGE nil)
(MESSAGE-TO-STANDARD-OUTPUT :disabled)
(METHOD-ARGLIST :disabled)
(METHOD-ARGLIST-FOR-SWANK :disabled)
(METHOD-DOCUMENTATION :disabled)
(MIDPOINT :disabled)
(MOVE movement)
(MOVE-TO movement)
(MOVE-TOWARD buffers)
(MOVE-WINDOW buffers)
(MOVE-WINDOW-TO buffers)
(MOVE-WINDOW-TO-NODE buffers)
(NORMALIZE-EVENT events)
(NUMBER-OF-JOYSTICKS joystick)
(OBJECT-EQ :disabled)
(OBJECTS :disabled)
(ON-SCREEN-P buffers)
(OPACITY :disabled)
(OPEN-PROJECT nil)
(OPPOSITE-DIRECTION movement)
(OPPOSITE-HEADING movement)
(PASTE buffers)
(PASTE-AS-NEW-BUFFER :disabled)
(PASTE-FROM buffers)
(PASTE-HERE :disabled)
(PASTE-INTO :disabled)
(PERCENT-GRAY :disabled)
(PERCENT-GREY :disabled)
(PERCENT-OF-TIME math)
(PLAY-MUSIC sound)
(PLAY-SAMPLE sound)
(PNODE :disabled)
(PRELOAD-RESOURCES resources)
(PRESS mouse)
(PROJECT-ORTHOGRAPHICALLY :disabled)
(QUADTREE :disabled)
(QUADTREE-COLLIDE collision-detection)
(QUADTREE-DELETE collision-detection)
(QUADTREE-DEPTH :disabled)
(QUADTREE-INSERT collision-detection)
(QUADTREE-NODE :disabled)
(QUIT nil)
(RADIAN-ANGLE math)
(RANDOM-CHOOSE math)
(READ-SEXP-FROM-FILE nil)
(RELEASE mouse)
(REMOVE-NODE buffers)
(RENDER-PLASMA math)
(RESET-JOYSTICK joystick)
(RESET-MESSAGE-FUNCTION hooks)
(RESIZE drawing)
(RESIZE-TO-BACKGROUND-IMAGE drawing)
(RESIZE-TO-IMAGE drawing)
(RESTORE-LOCATION :disabled)
(RIGHT-ANALOG-STICK-HEADING joystick)
(RIGHT-ANALOG-STICK-PRESSED-P joystick)
(RIGHT-ANALOG-STICK-PRESSURE joystick)
(RIGHT-OF :disabled)
(RIGHT-OF-CENTER :disabled)
(RUN-HOOK hooks)
(SAVE-EXCURSION buffers)
(SAVE-LOCATION :disabled)
(SCALE drawing)
(SCALE-BOUNDING-BOX :disabled)
(SCALE-WINDOW buffers)
(SCROLL-DOWN mouse)
(SCROLL-LEFT mouse)
(SCROLL-RIGHT mouse)
(SCROLL-TAP mouse)
(SCROLL-UP mouse)
(SECONDS->FRAMES math)
(SEEK-MUSIC sound)
(SEND-EVENT events)
(SET-BLENDING-MODE drawing)
(SET-MUSIC-VOLUME sound)
(SHUT-DOWN :disabled)
(SNAP-WINDOW-TO-NODE buffers)
(SPLIT-STRING-ON-LINES :disabled)
(STACK-HORIZONTALLY buffers)
(STACK-VERTICALLY buffers)
(START :disabled)
(START-SESSION :disabled)
(START-UP :disabled)
(STEP-COORDINATES :disabled)
(STEP-IN-DIRECTION movement)
(DIRECTION-TO movement)
(STEP-TOWARD-HEADING movement)
(STOP :disabled)
(STOP-FOLLOWING buffers)
(SWITCH-TO-BUFFER buffers)
(TAGS :disabled)
(TAP mouse)
(TOGGLE-DEBUG :disabled)
(TOUCHING-POINT-P collision-detection)
(TRANSFORM-WINDOW buffers)
(TRIM buffers)
(TURN-LEFT movement)
(TURN-RIGHT movement)
(UNBIND-EVENT events)
(UPDATE buffers)
(UPDATE-WINDOW-GLIDE :disabled)
(USE-FILTER drawing)
(VISIT :disabled)
(VISIBLE :disabled)
(WIDTH :disabled)
(WILL-OBSTRUCT-P pathfinding)
(WINDOW-BOUNDING-BOX buffers)
(WINDOW-POINTER-X buffers)
(WINDOW-POINTER-Y buffers)
(WINDOW-X :disabled)
(WINDOW-X0 :disabled)
(WINDOW-Y :disabled)
(WINDOW-Y0 :disabled)
(WINDOW-Z :disabled)
(WINDOW-Z0 :disabled)
(WITH-BORDER buffers)
(WITH-BUFFER buffers)
(WITH-FONT text)
(WITH-FIELDS :disabled)
(WITH-LOCAL-FIELDS :disabled)
(WITH-FIELDS-EX :disabled)
(WITH-FIELD-VALUES :disabled)
(WITH-NEW-BUFFER buffers)
(WITH-QUADTREE collision-detection)
(WITH-SESSION nil)
(WITHOUT-STYLE-WARNINGS :disabled)
(WRITE-SEXP-TO-FILE nil)
(X movement)
(XBLOCK :disabled)
(XELF :disabled)
(XELF-OBJECT :disabled)
(XELF-OBJECT-P :disabled)
(XELFP :disabled)
(XNODE :disabled)
(Y movement)
(ADD-BLOCK :disabled)
(CLONE :disabled)
(COMPOSE-BLANK-FIELDS :disabled)
(DEFINE :disabled)
(DEFPROJECT :disabled)
(DROP :disabled)
(DROP-NODE :disabled)
(FIELD-VALUE :disabled)
(HAS-TAG :disabled)
(HAS-NODE-P :disabled)
(NEW :disabled)
(PAUSE :disabled)
(PAUSED :disabled)
(SELF :disabled)
(SET-FIELD-VALUE :disabled)
(Z movement)))

;;; generics.lisp ends here

