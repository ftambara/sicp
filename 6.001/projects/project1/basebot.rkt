#lang racket

(provide position
         root1
         root2
         time-to-impact
         time-to-height
         max-input-between
         find-best-angle
         degree2radian
         travel-distance-simple
         travel-distance)

;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* (/ a 2.0) (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2

(define root1
  (lambda (a b c)
    (let ((discr (- (square b) (* 4.0 a c))))
      (if (negative? discr)
          false
          (/ (- (- b) (sqrt discr))
             (* 2.0 a))))))

(define root2
  (lambda (a b c)
    (let ((discr (- (square b) (* 4.0 a c))))
      (if (negative? discr)
          false
          (/ (+ (- b) (sqrt discr))
             (* 2.0 a))))))

;; complete these procedures and show some test cases

;; Problem 3

; Given that the acceleration factor will
; always be negative, the root1 procedure
; will produce the sensible result.
(define gravity-coherent-root root1)

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (gravity-coherent-root (- gravity) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define (velocity-x vel angle_rad)
  (* vel (cos angle_rad)))

(define (velocity-y vel angle_rad)
  (* vel (sin angle_rad)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
  (* (velocity-x velocity angle)
     (time-to-impact (velocity-y velocity angle) elevation))))

;; let's try this out for some example values.  Note that we are going to
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

; (time-to-impact (velocity-y 45 0) 1)
; ; => 0.3194

; (time-to-impact (velocity-y 45 (degree2radian 90)) 1)
; ; => 4.6140

; (time-to-impact (velocity-y 45 (degree2radian 45)) 1)
; ; => 3.2780

;; what is the distance traveled in each case?
;; record both in meters and in feet

; (travel-distance-simple 1 45 0)
; ; => 14.375

; (travel-distance-simple 1 45 (degree2radian 90))
; ; => 0.0003

; Should be exactly zero.
; The first non-zero digit gives us an
; idea of the maximum precision we can
; expect from our procedure.

; (travel-distance-simple 1 45 (degree2radian 45))
; ; => 104.31


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.001)
(define (echo x)
  (display x)
  (newline)
  x)

(define (max-input-between func start end step)
  (define (iter max-in max-out input)
    (if (>= input end)
        max-in
        (let ((test-val (func input))
              (next-in (+ input step)))
        (if (> test-val max-out)
            (iter input test-val next-in)
            (iter max-in max-out next-in)))))
    (iter start (func start) (+ start step)))

(define find-best-angle
  (lambda (velocity elevation)
    (max-input-between
      (lambda (angle_rad)
        (travel-distance-simple elevation velocity angle_rad))
      0
      (degree2radian 90)
      alpha-increment)))

;; find best angle
;; try for other velocities
;; try for other heights

; (find-best-angle 10 0)
; ; => .785 rad (44.98 deg)
; (find-best-angle 20 0)
; ; => .785 rad (44.98 deg)
; (find-best-angle 0 0)
; ; => 0.0
; (find-best-angle -5 0)
; ; => 0.0
; (find-best-angle -5 5)
; ; => 1.57 rad (89.95 deg)
; (find-best-angle 10 10)
; ; => .424 rad (24.29 deg)
; (find-best-angle 20 10)
; ; => .618 rad (35.41 deg)

; Results within .06 degrees of the optimal angles
; Al results make sense. Even the latter two, which surprised
; me a bit, make perfect sense after drawing some parabolas.

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor
;; that can be computed

;; We would like to again compute distance , but taking into account
;; drag.
;; Basically we can rework the equations to get four coupled linear
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define (integrate x0 y0 u0 v0 dt g m beta)
  (define (speed u v)
    (sqrt (+ (square u) (square v))))
  (define (iter x y u v)
    (if (and (<= y 0) (< v 0))
        x
        (iter (+ x (* u dt))
              (+ y (* v dt))
              (- u (* (/ 1 m) (speed u v) beta u dt))
              (- v (* (+ (* (/ 1 m) (speed u v) beta u) g) dt)))))
  (iter x0 y0 u0 v0))

(define (travel-distance elevation velocity angle_rad)
  (integrate
    0
    elevation
    (velocity-x velocity angle_rad)
    (velocity-y velocity angle_rad)
    alpha-increment
    gravity
    mass
    beta))


;; RUN SOME TEST CASES

; (travel-distance 1 45 (degree2radian 45))
; ; => 79.803
; (travel-distance-simple 1 45 (degree2radian 45))
; ; => 104.31
; (travel-distance 1 45 0)
; ; => 11.595
; (travel-distance-simple 1 45 0)
; ; => 14.375

;; what about Denver?

; We could replace the air density with the approximate
; value of 1.06 kg/m^3 and re-run the example cases
; (travel-distance 1 45 (degree2radian 45))
; ; => 86.735
; (travel-distance 1 45 0)
; ; => 12.220
; With less air resistance, the distances go up as expected

;; Problem 7

;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to
;; use, given a velocity, in order to reach a given height (receiver) at a
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft)
;; using 45m/s

(define max-distance-error .1)
(define (within? a b max-diff)
  (< (abs (- a b)) max-diff))

(define (travel-time elevation velocity angle-rad)
  (define (integrate x0 y0 u0 v0 dt g m beta)
    (define (speed u v)
      (sqrt (+ (square u) (square v))))
    (define (iter time x y u v)
      (if (and (<= y 0) (< v 0))
          time
          (iter (+ time dt)
                (+ x (* u dt))
                (+ y (* v dt))
                (- u (* (/ 1 m) (speed u v) beta u dt))
                (- v (* (+ (* (/ 1 m) (speed u v) beta u) g) dt)))))
    (iter 0 x0 y0 u0 v0))
  (integrate 0
             elevation
             (velocity-x velocity angle-rad)
             (velocity-y velocity angle-rad)
             alpha-increment
             gravity
             mass
             beta))

(define (find-best-throw-angle elevation velocity distance)
  (define (iter best-time best-angle angle-end angle-step angle-rad)
    (if (> angle-rad angle-end)
      best-angle
      (let ((test-travel
              (travel-distance elevation velocity angle-rad)))
        (if (within? test-travel distance max-distance-error)
            (let ((test-time (travel-time elevation velocity angle-rad)))
              (if (or (= best-time -1) (< test-time best-time))
                  (iter test-time angle-rad angle-end angle-step (+ angle-rad angle-step))
                  (iter best-time best-angle angle-end angle-step (+ angle-rad angle-step))))
            (iter best-time best-angle angle-end angle-step (+ angle-rad angle-step))))))
  (iter -1 -1 (degree2radian 90) alpha-increment (degree2radian -90)))


(define (radian->degree rad)
  (* (/ rad pi) 180))

(define (print-angle-and-time elevation velocity distance)
  (let ((angle (find-best-throw-angle elevation velocity distance)))
    (printf "y0=~a, v0=~a, d=~a: " elevation velocity distance)
    (if (= angle -1)
        (printf "no solution~%")
        ; Print the angle in radians and degrees
        ; with a precision of 4 digits
        (printf "~a rad (~a deg), time=~a~%"
                (~r angle #:precision 4)
                (~r (radian->degree angle) #:precision 4)
                (~r (travel-time elevation velocity angle) #:precision 4)))))

; (print-angle-and-time 1 45 30)
; ; y0=1, v0=45, d=30: 0.1882 rad (10.7834 deg), time=0.781
; (print-angle-and-time 1 45 36)
; ; y0=1, v0=45, d=36: 0.2402 rad (13.7627 deg), time=0.978
; (print-angle-and-time 1 45 60)
; ; y0=1, v0=45, d=60: 0.4602 rad (26.3678 deg), time=2.017
; (print-angle-and-time 1 45 90)
; ; y0=1, v0=45, d=90: no solution

; This last one was also tried with an increased
; allowed error of 1 m, and still there was no
; solution found, indicating that no angle can
; make a 45 m/s throw reach a 90 m target

;; Problem 8

(define (travel-distance-bounces elevation velocity angle-rad bounces)
  (define (integrate x0 y0 u0 v0 dt g m beta)
    (define (enough-bounces? num-bounces)
      (and (not (negative? bounces))
           (>= num-bounces bounces)))
    (define (stopped? u)
      (<= (abs u) 0.01))
    (define (speed u v)
      (sqrt (+ (square u) (square v))))
    (define (iter bounce-count x y u v)
      (if (and (<= y 0) (< v 0))
          ; Increase bounce count before comparing
          (if (or (stopped? u)
                  (enough-bounces? (+ bounce-count 1)))
              x
              (iter (+ bounce-count 1)
                    x
                    0
                    (/ u 2.0)
                    (- (/ v 2.0))))
          (iter bounce-count
                (+ x (* u dt))
                (+ y (* v dt))
                (- u (* (/ 1 m) (speed u v) beta u dt))
                (- v (* (+ (* (/ 1 m) (speed u v) beta u) g) dt)))))
    (iter 0 x0 y0 u0 v0))
  (integrate 0
             elevation
             (velocity-x velocity angle-rad)
             (velocity-y velocity angle-rad)
             alpha-increment
             gravity
             mass
             beta))

; (travel-distance-bounces 1 45 (degree2radian 45) 1)
; ; => 79.803
; (travel-distance-bounces 1 45 (degree2radian 45) 2)
; ; => 94.503
; (travel-distance-bounces 1 45 (degree2radian 45) 3)
; ; => 97.808

; (travel-distance-bounces 1 45 (degree2radian 25) 1)
; ; => 57.585
; (travel-distance-bounces 1 45 (degree2radian 25) 2)
; ; => 74.115
; (travel-distance-bounces 1 45 (degree2radian 25) 3)
; ; => 78.167

; (travel-distance-bounces 1 45 (degree2radian 65) 1)
; ; => 65.621
; (travel-distance-bounces 1 45 (degree2radian 65) 2)
; ; => 73.563
; (travel-distance-bounces 1 45 (degree2radian 65) 3)
; ; => 75.196

; (travel-distance-bounces 1 55 (degree2radian 25) 1)
; ; => 67.265
; (travel-distance-bounces 1 55 (degree2radian 25) 2)
; ; => 87.324
; (travel-distance-bounces 1 55 (degree2radian 25) 3)
; ; => 92.201

; The distance doesn't increase proportionally
; with the speed, which is expeted due to the
; air resistance increasing with the square of
; the speed.

; (travel-distance-bounces 1 45 (degree2radian 25) -1)
; ; => 79.519
; (travel-distance-bounces 1 45 (degree2radian 45) -1)
; ; => 98.885
; (travel-distance-bounces 1 45 (degree2radian 65) -1)
; ; => 75.715

;; Problem 9
