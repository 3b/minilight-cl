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
    (with-slots (ox oy oz dx dy dz) ray
      (multiple-value-bind (hit-p hit-dist hit-ref hit-position)
	  (intersect-p ray scene)
        (declare (ignore  hit-dist))
	(let ((-direction (vec3 (- dx) (- dy) (- dz))))
	  (if hit-p
	      (let* ((surface-point (make-surface-point hit-ref hit-position))
		     (origin (vec3 ox oy oz))
		     (local-emission (if last-hit
					 (vec3-0)
					 (emission surface-point
						   origin
						   (vec3 dx dy dz); -direction
						   nil)))
		     (illumination
		      (sample-emitters raytracer ray surface-point)))
		(multiple-value-bind (next-direction color)
		    (next-direction surface-point -direction)
		  (let ((reflection
			 (if (vector-zerop next-direction)
			     (vec3-0)
			     (vector* color
				      (radiance raytracer
						(apply #'make-slope-ray
						       (concatenate 'list
								    (^position surface-point)
								    next-direction))
						(triangle-ref surface-point))))))
		    (vector+ reflection illumination local-emission))))
	      (default-emission scene -direction)))))))

(defmethod sample-emitters ((raytracer raytracer) (ray ray) surface-point)
  (with-slots (scene) raytracer
    (with-slots (dx dy dz) ray
      (multiple-value-bind (emitter-position emitter-ref)
	  (emitter scene)
	(if emitter-ref
	    (let ((emit-direction (nnormalize
                                   (vector- emitter-position
                                            (^position surface-point)))))
	      (multiple-value-bind (hit-p hit-dist hit-ref hit-pos)
		  (intersect-p (apply #'make-slope-ray
				      (concatenate 'list
						   (^position surface-point)
						   emit-direction))
			       scene)
                (declare (ignore hit-pos hit-dist))
		(if (or (not hit-p) (equalp emitter-ref hit-ref))
		    (let ((emission-in (emission (make-surface-point
                                                  emitter-ref
                                                  emitter-position)
                                                 (^position surface-point)
                                                 (vector* emit-direction (vec3 -1 -1 -1))
                                                 t)))
                   ;;   (format t "~s~%" emission-in)
                      (reflection surface-point
				  emit-direction
				  (v3d::vector*scalar emission-in (float (emitters-count scene)))
				  (vec3 (- dx) (- dy) (- dz))))
		    (vec3-0)))))
	(vec3-0)))))

