;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;;
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;; http://www.hxa7241.org

(in-package #:minilight)

(defclass raytracer ()
  ((scene :initarg :scene :reader scene)))

(defun make-raytracer (scene)
  (make-instance 'raytracer :scene scene))

;; (defgeneric radiance (ray))
;; (defgeneric sample-emitters (ray))

(defmethod radiance ((raytracer raytracer) (ray ray) &optional last-hit)
  (with-slots (scene) raytracer
    (format t "ray=~s~%" ray)
    (with-slots (direction origin) ray
      (multiple-value-bind (hit-ref hit-position)
	  (intersect-p ray scene)
	(if hit-ref
	    (let* ((surface-point (make-surface-point hit-ref hit-position))
		   (local-emission (if last-hit
				       (vec3-0)
				       (emission surface-point
						 origin
						 (vector- direction)
						 nil)))
		   (illumination
		    (sample-emitters raytracer ray surface-point)))
	      (multiple-value-bind (next-direction color)
		  (next-direction surface-point (vector- direction))
		(let ((reflection
		       (if (vector-zerop next-direction)
			   (next-direction)
			   (vector* color
				    (radiance raytracer
					      (apply #'make-slope-ray
						     (concatenate 'list
								  (^position surface-point)
								  next-direction))
					      (triangle-ref surface-point))))))
		  (vector+ reflection illumination local-emission))))
	    (default-emission scene (vector- direction)))))))

(defmethod sample-emitters ((raytracer raytracer) (ray ray) surface-point)
  (with-slots (scene) raytracer
    (with-slots (origin direction) ray
      (multiple-value-bind (emitter-position emitter-ref)
	  (emitter scene)
       (if emitter-ref
	 (let ((emit-direction (vector- emitter-position
					(^position surface-point))))
	   (multiple-value-bind (hit-ref hit-pos)
	       (intersect-p (apply #'make-slope-ray
				   (concatenate 'list
						(^position surface-point)
						emit-direction))
			    scene)
	     (if (or (not hit-ref) (equalp emitter-ref hit-ref))
		 (let ((emission-in (make-surface-point emitter-ref
							emitter-position)))
		   (reflection surface-point
			       emit-direction
			       (* emission-in (emitters-count scene))
			       (vector- (direction ray))))
		 (vec3-0)))))
	 (vec3-0)))))

