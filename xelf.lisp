;;; xelf.lisp --- an emacs-inspired Lisp game engine

;; Copyright (C) 2006-2013 David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: multimedia, games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details. 

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This program is dedicated to our beloved Yogi, who died 2006-10-06.
;; Re-dedicated June 2012 to our beloved Cookie-Puss.

;;; Requirements:

;; See the included file INSTALL.

;;; Code:

(defpackage :xelf
    (:documentation "A 2d game engine in Common Lisp.")
  (:use :common-lisp) 
  (:export 
*after-load-project-hook*
*after-startup-hook*
*author*
*background-color*
*bold*
*buffer*
*buffers*
*cached-quadtree*
*colors*
*copyright-notice*
*current-directory*
*debug-on-error*
*default-frame-rate*
*default-joystick-profile*
*default-quadtree-depth*
*default-texture-filter*
*directions*
*event-handler-function*
*event-hook*
*executable*
*export-formats*
*font*
*font-texture-filter*
*font-texture-scale*
*frame-rate*
*frequency* 
*fullscreen*
*generic-joystick-mapping*
*gl-screen-height*
*gl-screen-width*
*italic*
*joystick-axis-dead-zone*
*joystick-axis-size*
*joystick-button-symbols*
*joystick-dead-zone*
*joystick-device*
*joystick-device-number*
*joystick-mapping*
*linux*
*message-function*
*message-history*
*message-hook-functions*
*message-logging*
*message-queue*
*monospace*
*next-update-hook*
*nominal-screen-height*
*nominal-screen-width*
*osx*
*output-channels* 
*output-chunksize* 
*persistent-variables*
*persistent-variables-file-name*
*pointer-x*
*pointer-y*
*preload-images*
*preload-samples*
*project*
*project-directories*
*project-path*
*quadtree*
*quadtree-depth*
*resizable*
*resize-hook*
*resource-handlers*
*resources*
*sans*
*scale-output-to-window*
*screen-height*
*screen-width*
*serif*
*update-function*
*updates*
*use-antialiased-text*
*use-sound*
*use-texture-blending*
*user-init-file-name*
*user-joystick-profile*
*user-keyboard-layout*
*user-projects-directory*
*window-position*
*window-title*
*windows*
*x11-color-data*
above
above-center
add-block
add-hook
add-node
add-resources
add-tag
add-to-list
aim
aim-at
align-to-pixels
alternate-tap
analog-stick-heading
analog-stick-pressed-p
analog-stick-pressure
location 
at-next-update
background-color
background-image
backward
below
below-center
bind-any-default-events
bind-event
bind-event
bind-event-to-method
blend
bounding-box
bounding-box-contains
buffer
build-quadtree
button-to-symbol
center-point
cfloat
change-image
clear-text-image-cache
clone
collide
colliding-with-bounding-box-p
colliding-with-p
colliding-with-rectangle-p
collision-type
color
compose
compose-below
compose-beside 
compose-blank-fields
compose-blank-fields
create-path
create-project-image
current-buffer
current-directory
default-events
default-project-directories
define
define-buffer
define-resource
defproject
defresource
defun-memo
depth
depth
destroy
direction-degrees
direction-heading
direction-heading
direction-to
direction-to
disable-key-repeat
distance
distance-between
draw
draw-box
draw-image
draw-line
draw-string
draw-string-blended
draw-string-shaded
draw-string-solid
draw-textured-rectangle
draw-textured-rectangle-*
drop
drop-node
duplicate
emptyp
enable-key-repeat
events
field-value
find-bounding-box
find-heading
find-object
find-path
find-path-waypoints
find-resource
find-resource-object
find-resource-property
find-text-image
find-texture
follow-with-camera
followed-object
font-height
font-text-width
font-text-width
font-width
forward
get-color
get-nodes
do-nodes
glide-follow
glide-window-to
glide-window-to-node
halt-music
halt-sample
handle-collision
handle-event
handle-point-motion
has-node-p
has-tag
heading quadtree-node
heading-between
heading-degrees
heading-direction
heading-to-cursor
height
holding-control
holding-shift
image
image-heading
image-height
image-width
index-all-images
index-all-samples
index-pending-resources
index-resource
initialize
insert
install-quadtree
is-joystick-event
is-raw-joystick-event
joystick-axis-pressed-p
joystick-axis-pressed-p
joystick-axis-raw-value
joystick-axis-raw-value
joystick-axis-value
joystick-axis-value
joystick-axis-value
joystick-button-pressed-p
joystick-button-pressed-p
joystick-button-state
joystick-button-state
joystick-buttons
joystick-event-p
joystick-left-analog-stick
joystick-name
joystick-profile
joystick-profile
joystick-right-analog-stick
joystick-type
keyboard-down-p
keyboard-held-p
keyboard-keys-down
keyboard-modifier-down-p
keyboard-modifiers
keyboard-pressed-p
keyboard-released-p
keyboard-time-in-current-state
keyboard-time-in-previous-state
kill-buffer
last-x last-y last-z
left-analog-stick-heading
left-analog-stick-pressed-p
left-analog-stick-pressure
left-of
left-of-center
load-project-image
load-resource
load-user-init-file
load-variables
make-directory-maybe
make-event
make-keyword
make-non-keyword
message
message-to-standard-output
method-arglist
method-arglist-for-swank
method-documentation
midpoint
move
move-to
move-toward
move-window
move-window-to
move-window-to-node
new
node 
normalize-event
number-of-joysticks
object-eq
objects
on-screen-p
opacity
open-project
opposite-direction
opposite-heading
paste
paste
paste-as-new-buffer
paste-from
paste-here
paste-into
pause
paused
percent-gray
percent-grey
percent-of-time
persistent-variables-file
pinned
play-music
play-sample
pnode
preload-resources
press
project-orthographically
quadtree
quadtree-collide
quadtree-delete
quadtree-depth
quadtree-insert
quadtree-node
quit
radian-angle
random-choose
read-sexp-from-file
release
remove-block
remove-node
remove-tag
render-plasma
reset-joystick
reset-message-function
resize
resize-to-background-image
resize-to-image
restore-location
right-analog-stick-heading
right-analog-stick-pressed-p
right-analog-stick-pressure
right-of
right-of-center
run-hook
save-excursion
save-location
scale
scale-bounding-box
scale-window
scroll-down
scroll-left
scroll-right
scroll-tap
scroll-up
seconds->frames
seek-music
self
send-event
set-blending-mode
set-field-value
set-music-volume
shut-down
snap-window-to-node
split-string-on-lines
stack-horizontally
stack-vertically
start
start-session
start-up
step-coordinates
step-in-direction
step-toward-heading
stop
stop-following
switch-to-buffer
tags
tap 
toggle-debug
touching-point-p
transform-window
trim
turn-left
turn-right
unbind-event
update
update-window-glide
use-filter
visible
visit
width
will-obstruct-p
window-bounding-box
window-pointer-x
window-pointer-y
window-x
window-x
window-x0
window-y
window-y
window-y0
window-z
window-z0
with-border
with-buffer
with-field-values
with-fields
with-fields-ex
with-font
with-local-fields
with-new-buffer
with-quadtree
with-session
without-style-warnings
write-sexp-to-file
x y z
xblock
xelf
xelf-object
xelf-object-p
xelfp
xnode
))

