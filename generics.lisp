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

(defgeneric aim-at-thing
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

(defgeneric direction-to-thing
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
'(
(*AFTER-LOAD-PROJECT-HOOK* hooks)
(*AFTER-STARTUP-HOOK* hooks)
(*AUTHOR* nil)
(*BACKGROUND-COLOR* nil)
(*BOLD* text)
(*BUFFER* buffers)
(*BUFFERS* nil)
(*CACHED-QUADTREE* collision-detection)
(*COLORS* drawing)
(*COPYRIGHT-NOTICE* nil)
(*CURRENT-DIRECTORY* nil)
(*DEBUG-ON-ERROR* nil)
(*DEFAULT-FRAME-RATE* nil)
(*DEFAULT-JOYSTICK-PROFILE* joystick)
(*DEFAULT-QUADTREE-DEPTH* collision-detection)
(*DEFAULT-TEXTURE-FILTER* drawing)
(*DIRECTIONS* math)
(*EVENT-HANDLER-FUNCTION* hooks)
(*EVENT-HOOK* hooks)
(*EXECUTABLE* nil)
(*EXPORT-FORMATS* nil)
(*FONT* text)
(*FONT-TEXTURE-FILTER* text)
(*FONT-TEXTURE-SCALE* text)
(*FRAME-RATE* nil)
(*FREQUENCY* sound)
(*FULLSCREEN* nil)
(*GENERIC-JOYSTICK-MAPPING* joystick)
(*GL-SCREEN-HEIGHT* nil)
(*GL-SCREEN-WIDTH* nil)
(*ITALIC* text)
(*JOYSTICK-AXIS-DEAD-ZONE* joystick)
(*JOYSTICK-AXIS-SIZE* joystick)
(*JOYSTICK-BUTTON-SYMBOLS* joystick)
(*JOYSTICK-DEAD-ZONE* joystick)
(*JOYSTICK-DEVICE* joystick)
(*JOYSTICK-DEVICE-NUMBER* joystick)
(*JOYSTICK-MAPPING* joystick)
(*LINUX* nil)
(*MESSAGE-FUNCTION* hooks)
(*MESSAGE-HISTORY* nil)
(*MESSAGE-HOOK-FUNCTIONS* hooks)
(*MESSAGE-LOGGING* nil)
(*MESSAGE-QUEUE* nil)
(*MONOSPACE* text)
(*NEXT-UPDATE-HOOK* hooks)
(*NOMINAL-SCREEN-HEIGHT* drawing)
(*NOMINAL-SCREEN-WIDTH* drawing)
(*OSX* nil)
(*OUTPUT-CHANNELS* sound)
(*OUTPUT-CHUNKSIZE* sound)
(*PERSISTENT-VARIABLES* nil)
(*PERSISTENT-VARIABLES-FILE-NAME* nil)
(*POINTER-X* mouse)
(*POINTER-Y* mouse)
(*PRELOAD-IMAGES* nil)
(*PRELOAD-SAMPLES* nil)
(*PROJECT* nil)
(*PROJECT-DIRECTORIES* nil)
(*PROJECT-PATH* nil)
(*QUADTREE* collision-detection)
(*QUADTREE-DEPTH* collision-detection)
(*RESIZABLE* nil)
(*RESIZE-HOOK* hooks)
(*RESOURCE-HANDLERS* nil)
(*RESOURCES* nil)
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
(*USER-INIT-FILE-NAME* nil)
(*USER-JOYSTICK-PROFILE* joystick)
(*USER-KEYBOARD-LAYOUT* nil)
(*USER-PROJECTS-DIRECTORY* nil)
(*WINDOW-POSITION* drawing)
(*WINDOW-TITLE* drawing)
(*WINDOWS* nil)
(*X11-COLOR-DATA* drawing)
(ABOVE nil)
(ABOVE-CENTER nil)
(ADD-HOOK hooks)
(ADD-NODE buffers)
(ADD-RESOURCES nil)
(ADD-TAG nil)
(ADD-TO-LIST hooks)
(AIM movement)
(AIM-AT-THING movement)
(ALIGN-TO-PIXELS drawing)
(ALTERNATE-TAP mouse)
(ANALOG-STICK-HEADING joystick)
(ANALOG-STICK-PRESSED-P joystick)
(ANALOG-STICK-PRESSURE joystick)
(AT-NEXT-UPDATE nil)
(BACKGROUND-COLOR drawing)
(BACKGROUND-IMAGE drawing)
(BACKWARD movement)
(BELOW nil)
(BELOW-CENTER nil)
(BIND-ANY-DEFAULT-EVENTS nil)
(BIND-EVENT events)
(BIND-EVENT-TO-METHOD events)
(BLEND drawing)
(BOUNDING-BOX collision-detection)
(BOUNDING-BOX-CONTAINS collision-detection)
(BUFFER buffers)
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
(COLLISION-TYPE collision-detection)
(COLOR drawing)
(COMPOSE buffers)
(COMPOSE-BELOW buffers)
(COMPOSE-BESIDE buffers)
(CREATE-PATH pathfinding)
(CREATE-PROJECT-IMAGE nil)
(CURRENT-BUFFER buffers)
(CURRENT-DIRECTORY nil)
(DEFAULT-EVENTS nil)
(DEFAULT-PROJECT-DIRECTORIES nil)
(DEFINE-BUFFER buffers)
(DEFINE-RESOURCE resources)
(DEFRESOURCE resources)
(DEFUN-MEMO nil)
(DEPTH nil)
(DESTROY lifecycle)
(DIRECTION-DEGREES movement)
(DIRECTION-HEADING movement)
(DIRECTION-TO movement)
(DIRECTION-TO-THING movement)
(DISABLE-KEY-REPEAT keyboard)
(DISTANCE movement)
(DISTANCE-BETWEEN movement)
(DO-NODES buffers)
(DRAW drawing)
(DRAW-BOX drawing)
(DRAW-IMAGE drawing)
(DRAW-LINE drawing)
(DRAW-STRING drawing)
(DRAW-STRING-BLENDED drawing)
(DRAW-STRING-SHADED drawing)
(DRAW-STRING-SOLID drawing)
(DRAW-TEXTURED-RECTANGLE drawing)
(DRAW-TEXTURED-RECTANGLE-* drawing)
(DUPLICATE nil)
(EMPTYP buffers)
(ENABLE-KEY-REPEAT keyboard)
(EVENTS events)
(FIND-BOUNDING-BOX movement)
(FIND-HEADING movement)
(FIND-OBJECT nil)
(FIND-PATH pathfinding)
(FIND-PATH-WAYPOINTS pathfinding)
(FIND-RESOURCE resources)
(FIND-RESOURCE-OBJECT resources)
(FIND-RESOURCE-PROPERTY resources)
(FIND-TEXT-IMAGE text)
(FIND-TEXTURE drawing)
(FOLLOW-WITH-CAMERA buffers)
(FOLLOWED-OBJECT buffers)
(FONT-HEIGHT text)
(FONT-TEXT-WIDTH text)
(FONT-WIDTH text)
(FORWARD movement)
(GET-COLOR drawing)
(GET-NODES buffers)
(GLIDE-FOLLOW buffers)
(GLIDE-WINDOW-TO buffers)
(GLIDE-WINDOW-TO-NODE buffers)
(HALT-MUSIC sound)
(HALT-SAMPLE sound)
(HANDLE-COLLISION collision-detection)
(HANDLE-EVENT events)
(HANDLE-POINT-MOTION mouse)
(HAS-NODE-P buffers)
(HEADING movement)
(HEADING-BETWEEN movement)
(HEADING-DEGREES movement)
(HEADING-DIRECTION movement)
(HEADING-TO-CURSOR movement)
(HEADING-TO-THING movement)
(HEIGHT movement)
(HOLDING-CONTROL keyboard)
(HOLDING-SHIFT keyboard)
(IMAGE drawing)
(IMAGE-HEADING drawing)
(IMAGE-HEIGHT drawing)
(IMAGE-WIDTH drawing)
(INDEX-ALL-IMAGES resources)
(INDEX-ALL-SAMPLES resources)
(INDEX-PENDING-RESOURCES resources)
(INDEX-RESOURCE resources)
(INITIALIZE nil)
(INSERT nil)
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
(KILL-BUFFER nil)
(LAST-X nil)
(LAST-Y nil)
(LAST-Z nil)
(LEFT-ANALOG-STICK-HEADING joystick)
(LEFT-ANALOG-STICK-PRESSED-P joystick)
(LEFT-ANALOG-STICK-PRESSURE joystick)
(LEFT-OF nil)
(LEFT-OF-CENTER nil)
(LOAD-PROJECT-IMAGE nil)
(LOAD-RESOURCE resources)
(LOAD-USER-INIT-FILE nil)
(LOAD-VARIABLES nil)
(LOCATION movement)
(MAKE-DIRECTORY-MAYBE nil)
(MAKE-EVENT events)
(MAKE-KEYWORD nil)
(MAKE-NON-KEYWORD nil)
(MESSAGE nil)
(MESSAGE-TO-STANDARD-OUTPUT nil)
(METHOD-ARGLIST nil)
(METHOD-ARGLIST-FOR-SWANK nil)
(METHOD-DOCUMENTATION nil)
(MIDPOINT movement)
(MOVE movement)
(MOVE-TO movement)
(MOVE-TOWARD buffers)
(MOVE-WINDOW buffers)
(MOVE-WINDOW-TO buffers)
(MOVE-WINDOW-TO-NODE buffers)
(NORMALIZE-EVENT events)
(NUMBER-OF-JOYSTICKS joystick)
(OBJECT-EQ nil)
(OBJECTS buffers)
(ON-SCREEN-P buffers)
(OPACITY drawing)
(OPEN-PROJECT nil)
(OPPOSITE-DIRECTION movement)
(OPPOSITE-HEADING movement)
(PASTE buffers)
(PASTE-AS-NEW-BUFFER buffers)
(PASTE-FROM buffers)
(PASTE-HERE buffers)
(PASTE-INTO buffers)
(PERCENT-GRAY drawing)
(PERCENT-GREY drawing)
(PERCENT-OF-TIME math)
(PLAY-MUSIC sound)
(PLAY-SAMPLE sound)
(PNODE pathfinding)
(PRELOAD-RESOURCES resources)
(PRESS mouse)
(PROJECT-ORTHOGRAPHICALLY buffers)
(QUADTREE collision-detection)
(QUADTREE-COLLIDE collision-detection)
(QUADTREE-DELETE collision-detection)
(QUADTREE-DEPTH collision-detection)
(QUADTREE-INSERT collision-detection)
(QUADTREE-NODE collision-detection)
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
(RESTORE-LOCATION nil)
(RIGHT-ANALOG-STICK-HEADING joystick)
(RIGHT-ANALOG-STICK-PRESSED-P joystick)
(RIGHT-ANALOG-STICK-PRESSURE joystick)
(RIGHT-OF nil)
(RIGHT-OF-CENTER nil)
(RUN-HOOK hooks)
(SAVE-EXCURSION buffers)
(SAVE-LOCATION nil)
(SCALE drawing)
(SCALE-BOUNDING-BOX nil)
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
(SHUT-DOWN nil)
(SNAP-WINDOW-TO-NODE buffers)
(SPLIT-STRING-ON-LINES nil)
(STACK-HORIZONTALLY buffers)
(STACK-VERTICALLY buffers)
(START nil)
(START-SESSION nil)
(START-UP nil)
(STEP-COORDINATES movement)
(STEP-IN-DIRECTION movement)
(STEP-TOWARD-HEADING movement)
(STOP nil)
(STOP-FOLLOWING buffers)
(SWITCH-TO-BUFFER buffers)
(TAGS nil)
(TAP nil)
(TOGGLE-DEBUG nil)
(TOUCHING-POINT-P collision-detection)
(TRANSFORM-WINDOW buffers)
(TRIM buffers)
(TURN-LEFT movement)
(TURN-RIGHT movement)
(UNBIND-EVENT events)
(UPDATE buffers)
(UPDATE-WINDOW-GLIDE buffers)
(USE-FILTER drawing)
(VISIT buffers)
(WIDTH nil)
(WILL-OBSTRUCT-P pathfinding)
(WINDOW-BOUNDING-BOX buffers)
(WINDOW-POINTER-X buffers)
(WINDOW-POINTER-Y buffers)
(WINDOW-X buffers)
(WINDOW-X0 buffers)
(WINDOW-Y buffers)
(WINDOW-Y0 buffers)
(WINDOW-Z buffers)
(WINDOW-Z0 buffers)
(WITH-BORDER buffers)
(WITH-BUFFER buffers)
(WITH-FONT text)
(WITH-NEW-BUFFER buffers)
(WITH-QUADTREE collision-detection)
(WITH-SESSION nil)
(WITHOUT-STYLE-WARNINGS nil)
(WRITE-SEXP-TO-FILE nil)
(X movement)
(XBLOCK nil)
(XELF nil)
(XELF-OBJECT nil)
(XELF-OBJECT-P nil)
(XELFP nil)
(XNODE nil)
(Y movement)
(Z movement)))

;;; generics.lisp ends here

