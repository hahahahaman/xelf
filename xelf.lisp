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
add-object
add-resources
add-tag
add-to-list
aim
aim-at-thing
align-to-pixels
alternate-tap
analog-stick-heading
analog-stick-pressed-p
analog-stick-pressure
at
at-next-update
backward
below
below-center
bind-event
bind-event
bind-event-to-method
bounding-box
bounding-box-contains
build-quadtree
button-to-symbol
center-point
cfloat
change-image
clear-text-image-cache
clone
collide
colliding-with
colliding-with
colliding-with-bounding-box
colliding-with-rectangle-p
compose
compose-below
compose-beside 
compose-blank-fields
compose-blank-fields
create-path
create-project-image
current-buffer
current-directory
default-project-directories
defblock
define
define-buffer
define-resource
defproject
defresource
defun-memo
destroy
direction-degrees
direction-heading
direction-heading
direction-to
direction-to-thing
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
drop-object
emptyp
enable-key-repeat
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
font-height
font-text-width
font-text-width
font-width
forward
get-color
get-objects
glide-follow
glide-window-to
glide-window-to-object
halt-music
halt-sample
handle-collision
handle-event
handle-point-motion
has-object-p
has-tag
heading-between
heading-degrees
heading-direction
heading-to-cursor
heading-to-thing
heading-to-thing
holding-control
holding-shift
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
joystick-axis-raw-value
joystick-axis-value
joystick-axis-value
joystick-button-pressed-p
joystick-button-state
joystick-profile
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
left-analog-stick-heading
left-analog-stick-pressed-p
left-analog-stick-pressure
left-of
left-of-center
load-project
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
move-window-to-object
new
normalize-event
number-of-joysticks
object-eq
on-screen-p
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
percent-gray
percent-grey
percent-of-time
persistent-variables-file
play-music
play-sample
preload-resources
press
quadtree-collide
quadtree-delete
quadtree-insert
quit
radian-angle
random-choose
read-sexp-from-file
release
remove-block
remove-object
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
set-music-volume
shut-down
snap-window-to-object
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
visit
will-obstruct
window-bounding-box
window-pointer-x
window-pointer-y
window-x
window-y
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
xblock
xelf
xelf-object
xelf-object-p
xelfp
))

