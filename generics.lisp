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

(defgeneric at
  (NODE)
  (:documentation "Return as values the X,Y position of NODE."))

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
  
  (bind-event self '(:up) 'move-up)
  (bind-event self '(:down) 'move-down)
  (bind-event self '(:q :control) 'quit)
  (bind-event self '(:escape :shift) 'menu)

See also `keys.lisp' for the full table of key and modifier symbols.
  (:documentation "))

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
  (NODE DEGREES)
  (:documentation "Increase heading by DEGREES."))

(defgeneric turn-right
  (NODE DEGREES)
  (:documentation "Decrease heading by DEGREES."))

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

