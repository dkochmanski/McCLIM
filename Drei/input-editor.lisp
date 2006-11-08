;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Implementation of various bits and parts needed for Drei to
;;; function as the input-editor of McCLIM. Meaning, this is an
;;; interface between input-editing-streams and Drei instances. We
;;; also try not to mess too much with CLIM-INTERNALS to be somewhat
;;; portable (but not too much).

(in-package :drei)

;; Note that we use `stream-scan-pointer' to access the scan pointer
;; of the stream in the protocol methods, despite the fact that the
;; `drei-input-editing-mixin' class does not have a scan pointer. We
;; assume that the subclass defines a scan pointer.
(defclass drei-input-editing-mixin ()
  ((%drei-instance :accessor drei-instance
                   :initarg :drei-instance)
   (%input-position :accessor input-position
                    :initform 0)
   (%activation-gesture :accessor activation-gesture
                        :initform nil)
   (%rescanning-p :reader stream-rescanning-p
                  :writer (setf stream-rescanning)
                  :initform nil))
  (:documentation "An mixin that helps in implementing Drei-based
input-editing streams. This class should not be directly
instantiated."))

(defmethod initialize-instance :after ((obj drei-input-editing-mixin)
				       &rest args
				       &key stream (initial-contents "")
				       (cursor-visibility t)
                                       (min-width 0))
  (check-type min-width (or (integer 0) (eql t)))
  (check-type stream clim-stream-pane)
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (let ((max-width (- (stream-text-margin stream) cx)))
      (with-keywords-removed (args (:initial-contents))
	(setf (drei-instance obj)
	      (apply #'make-instance
		     'drei-area
		     :editor-pane stream
		     :buffer (make-instance 'drei-buffer
                                            :name "Input-editor buffer"
                                            :initial-contents initial-contents)
		     :x-position cx
		     :y-position cy
		     :active cursor-visibility
		     :max-width max-width
                     :minibuffer (or *minibuffer*
                                     *pointer-documentation-output*)
                     :allow-other-keys t
		     args)))
      (update-syntax (buffer (drei-instance obj))
                     (syntax (buffer (drei-instance obj))))
      ;; XXX Really add it here?
      (stream-add-output-record stream (drei-instance obj))
      (display-drei-area (drei-instance obj)))))

(defmethod stream-insertion-pointer
    ((stream drei-input-editing-mixin))
  (offset (point (drei-instance stream))))

(defmethod (setf stream-insertion-pointer)
    ((new-value integer) (stream drei-input-editing-mixin))
  (let* ((drei (drei-instance stream)))
    (setf (offset (point drei)) new-value)))

(defmethod cursor-visibility ((stream drei-input-editing-mixin))
  (active (point-cursor (drei-instance stream))))

(defmethod (setf cursor-visibility)
    (visibility (stream drei-input-editing-mixin))
  (setf (active (drei-instance stream)) visibility))

(defclass drei-unselectable-presentation (presentation)
  ()
  (:documentation "A presentation that will not be highlightable,
and can thus be safely used for implementing stuff such as noise
strings."))

(define-presentation-translator unselectable-presentation-to-nothing
    (drei-unselectable-presentation t global-command-table
                                    :menu nil
                                    :tester ((object)
                                             (declare (ignore object))))
    (object)
  (declare (ignore object)))

(defclass noise-string (drei-unselectable-presentation)
  ((%string :initarg :string
            :initform (error "A noise string must represent some string.")
            :reader noisy-string)
   (%text-style :initarg :text-style
                :initform (make-text-style :serif :italic nil)
                :reader text-style))
  (:documentation "Buffer objects of this class will be skipped
by the input-editor gesture reader. They should not be used
outside the input editor."))

(define-presentation-method present ((object noise-string) (type noise-string)
                                     stream (view drei-textual-view) &key &allow-other-keys)
  (with-text-style (stream (text-style object))
    (princ (noisy-string object) stream)))

(defclass accept-result (presentation)
  ((%object :initarg :object
            :initform (error "An object must be provided for the accept result.")
            :reader object)
   (%result-type :initarg :result-type
                 :initform t
                 :reader result-type))
  (:documentation "Buffer objects of this class are inserted into
the buffer when the user clicks on an applicable presentation
while in an input context for the input-editor. They should not
be used outside the input-editor."))

(define-presentation-method present (object (type accept-result) stream
                                            (view drei-textual-view) &rest args
                                            &key)
  (apply #'present (object object) (result-type object) :stream stream :view view args))

(defmethod prompt-for-accept :around ((stream drei-input-editing-mixin) type view
                                      &rest args &key &allow-other-keys)
  (declare (ignore args))
  ;; XXX: In Drei, the "input position" (a lovably underspecified
  ;; concept in the CLIM spec) is just after any input prompt. We do
  ;; not set the input position (or print the prompt) if we are
  ;; already at the input position or if we are rescanning. This is so
  ;; we can support fancy accept methods such as the one for
  ;; `command-or-form'
  (unless (or (stream-rescanning-p stream)
              (= (stream-scan-pointer stream) (input-position stream)))
    (call-next-method)
    ;; We skip ahead of any noise strings to put us past the
    ;; prompt. This is safe, because the noise strings are to be
    ;; ignored anyway, but we need to be ahead to set the input
    ;; position properly (ie. after the prompt).
    (loop
       with buffer = (buffer (drei-instance stream))
       until (= (stream-scan-pointer stream) (size buffer))
       while (typep (buffer-object buffer (stream-scan-pointer stream))
                    'noise-string)
       do (incf (stream-scan-pointer stream)))
    (setf (input-position stream) (stream-scan-pointer stream))))

(defmethod stream-accept :after ((stream drei-input-editing-mixin) type &key &allow-other-keys)
  ;; If we end up asking for more input using the stream, we do not
  ;; want to permit the user to undo input for this context.
  (clear-undo-history (buffer (drei-instance stream))))

(defmethod stream-input-buffer ((stream drei-input-editing-mixin))
  ;; NOTE: This is very slow, we should attempt to replace uses of
  ;; this function in McCLIM with something more efficient.
  (with-accessors ((buffer buffer)) (drei-instance stream)
    (let* ((array (buffer-sequence buffer 0 (size buffer))))
      (make-array (length array)
                  :fill-pointer t
                  :adjustable t
                  :initial-contents array))))

(defmethod replace-input ((stream drei-input-editing-mixin) (new-input array)
			  &key
			  (start 0)
			  (end (length new-input))
			  (buffer-start (input-position stream))
			  rescan)
  (check-type start integer)
  (check-type end integer)
  (check-type buffer-start integer)
  (let* ((drei (drei-instance stream))
         (new-contents (subseq new-input start end))
         (old-contents (buffer-sequence (buffer drei)
                                        buffer-start
                                        (stream-scan-pointer stream)))
         (equal (and (= (length new-contents)
                        (length old-contents))
                     (every #'equal new-contents old-contents))))
    (let ((begin-mark (clone-mark (point drei))))
      (unless equal
        (setf (offset begin-mark) buffer-start)
        (delete-region begin-mark (stream-scan-pointer stream))
        (insert-sequence begin-mark new-contents))
      (update-syntax (buffer drei) (syntax (buffer drei)))
      (display-drei-area drei)
      (when (or rescan (not equal))
        (queue-rescan stream)))))

(defun present-acceptably-to-string (object type view for-context-type)
  "Return two values - a string containing the printed
representation of `object' when presented with `type' and `view',
and an object. The second value will be NIL if the string is
\"acceptable\", that is, acceptable as input to the accept method
for `type', or `object' if it isn't."
  (flet ((present-it (acceptably)
	   (present-to-string object type
			      :view view
			      :acceptably acceptably
			      :for-context-type for-context-type)))
    (let* ((acceptably t)
	   (printed-rep nil))
      (handler-case
	  (setq printed-rep (present-it t))
	(error ()
	  (setq acceptably nil)
	  (setq printed-rep (present-it nil))))
      (values printed-rep (if acceptably
			      nil
			      object)))))

(defmethod presentation-replace-input
    ((stream drei-input-editing-mixin) object type view
     &rest args &key (buffer-start (input-position stream))
     rescan query-identifier (for-context-type type) (accept-result t))
  (declare (ignore query-identifier buffer-start rescan))
  ;; If the input is non-readable and `accept-result' is non-NIL, we
  ;; insert an `accept-result' object into the buffer, otherwise we
  ;; just insert the object itself. This is a non-specified
  ;; convenience extension (so we have to use :allow-other-keys t when
  ;; using it).
  (with-keywords-removed (args (:type :view :query-identifier :for-context-type))
    (multiple-value-bind (printed-rep accept-object)
        (present-acceptably-to-string object type view for-context-type)
      (apply #'replace-input stream
             (if accept-object
                 (vector (if accept-result
                             (make-instance 'accept-result
                                            :object accept-object
                                            :result-type type)
                             accept-object))
                 printed-rep)
             args))))

;;; Have to reexamine how many of the keyword arguments to
;;; stream-read-gesture should really be passed to the encapsulated
;;; stream.
;;;
;;; OK, now I know :) They should all be passed, except for peek-p.
;;; However, the loop that calls stream-read-gesture on the
;;; encapsulated stream needs to return null if we see a :timeout or
;;; :eof.
;;;
;;; Activation gesture handling has been moved out of
;;; stream-process-gesture to stream-read-gesture and
;;; stream-unread-gesture. This allows a gesture to be read in while
;;; it is not an activation gesture, unread, and then read again as an
;;; activation gesture. This kind of game seems to be needed for
;;; reading forms properly. -- moore
(defmethod stream-read-gesture ((stream drei-input-editing-mixin)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (with-keywords-removed (rest-args (:peek-p))
    (rescan-if-necessary stream)
    (with-accessors ((buffer stream-input-buffer)
                     (insertion-pointer stream-insertion-pointer)
                     (scan-pointer stream-scan-pointer)
                     (activation-gesture activation-gesture)) stream
      (loop
         (loop
            while (< scan-pointer insertion-pointer)
            while (< scan-pointer (length buffer))
            do (let ((gesture (aref buffer scan-pointer)))
                 ;; Skip noise strings.
                 (cond ((typep gesture 'noise-string)
                        (incf scan-pointer))
                       ((and (not peek-p)
                             (typep gesture 'accept-result))
                        (incf scan-pointer)
                        #+(or mcclim building-mcclim)
                        (climi::throw-object-ptype (object gesture)
                                                   (result-type gesture)))
                       ;; Note that this implies that
                       ;; `stream-read-gesture' may return accept
                       ;; results, which might as well be arbitrary
                       ;; objects to the code calling
                       ;; `stream-read-gesture', since it can't really
                       ;; do anything with them except for asserting
                       ;; that they exist. According to the spec,
                       ;; "accept results are treated as a single
                       ;; gesture", and this kind of behavior is
                       ;; necessary to make sure `stream-read-gesture'
                       ;; doesn't simply claim that there are no more
                       ;; gestures in the input-buffer when the
                       ;; remaining gesture(s) is an accept result.
                       ((typep gesture 'accept-result)
                        (return-from stream-read-gesture gesture))
                       (t
                        (unless peek-p
                          (incf scan-pointer))
                        (return-from stream-read-gesture gesture))
                       (t (incf scan-pointer)))))
         (setf (stream-rescanning stream) nil)
         (when activation-gesture
           (return-from stream-read-gesture
             (prog1 activation-gesture
               (unless peek-p
                 (setf activation-gesture nil)))))
         ;; In McCLIM, stream-process-gesture is responsible for
         ;; inserting characters into the buffer, changing the
         ;; insertion pointer and possibly setting up the
         ;; activation-gesture slot.
         (loop
            with gesture and type
            do (setf (values gesture type)
                     (apply #'stream-read-gesture
                            (encapsulating-stream-stream stream) rest-args))
            when (null gesture)
            do (return-from stream-read-gesture (values gesture type))
            when (stream-process-gesture stream gesture type)
            do (loop-finish))))))

(defmethod stream-unread-gesture ((stream drei-input-editing-mixin)
				  gesture)
  (with-accessors ((buffer stream-input-buffer)
                   (scan-pointer stream-scan-pointer)
                   (activation-gesture activation-gesture)) stream
    (when (> scan-pointer 0)
      (if (and (eql scan-pointer (fill-pointer buffer))
               (activation-gesture-p gesture))
          (setf activation-gesture gesture)
          (decf scan-pointer)))))

(defun read-gestures-and-act (stream first-gesture type)
  "Read gestures from `stream' and act upon them as per the
semantics of `process-gesture'. This basically means that we read
gestures and process a command, returning NIL if we don't
consider it an \"editing command\", rescan if it changed
something before the scan pointer, and just return the gesture if
it inserted stuff after the scan pointer. `First-gesture' must be
the gesture that will be read in the first call to
`stream-read-gesture' for the stream encapsulated by
`stream'. The second return value of this function will be `type'
if stuff is inserted after the insertion pointer."
  (let* ((before (stream-input-buffer stream))
         (drei (drei-instance stream))
         (*command-processor* drei)
         (was-directly-processing (directly-processing-p drei))
         (minibuffer (or (minibuffer drei) *minibuffer*)))
    (with-bound-drei-special-variables (drei
                                        ;; If the minibuffer is the
                                        ;; stream we are encapsulating
                                        ;; for the
                                        ;; input-editing-stream, we
                                        ;; don't want to use it as a
                                        ;; minibuffer.
                                        :minibuffer (if (eq minibuffer *standard-input*)
                                                        *pointer-documentation-output*
                                                        minibuffer)
                                        :prompt "M-x ")
      ;; We narrow the buffer to the input position, so the user won't
      ;; be able to erase the original command (when entering command
      ;; arguments) or stuff like argument prompts.
      (drei-core:with-narrowed-buffer (drei (input-position stream) t t)
        (handler-case (process-gestures-or-command drei)
          (unbound-gesture-sequence (c)
            (display-message "~A is unbound" (gesture-name (gestures c))))
          (abort-gesture (c)
            (if (member (abort-gesture-event c)
                        *abort-gestures*
                        :test #'event-matches-gesture-name-p)
                (signal 'abort-gesture :event (abort-gesture-event c))
                (when was-directly-processing
                  (display-message "Aborted"))))))
      ;; Will also take care of redisplaying minibuffer.
      (display-drei (pane-frame (editor-pane drei)) drei)
      (let ((first-mismatch (mismatch before (stream-input-buffer stream))))
        (cond ((null first-mismatch)
               ;; No change actually took place, even though IP may
               ;; have moved.
               nil)
              ((< first-mismatch (stream-scan-pointer stream))
               ;; Eek, change before scan pointer - this probably
               ;; changes the scan, so we'll have to rescan
               ;; everything. Bummer!
               (immediate-rescan stream))
              (t
               ;; Something happened, but since we haven't even gotten
               ;; to scanning that part of the buffer yet, it doesn't
               ;; really matter. All that matters is that something
               ;; happened, and that it modified the buffer. This is a
               ;; somewhat liberal reading of the CLIM spec.
               (values first-gesture type)))))))

(defmethod stream-process-gesture ((stream drei-input-editing-mixin)
				   gesture type)
  ;; If some other command processor has taken control, we do not want
  ;; to assume that an activation gesture really is an activation
  ;; gesture. For example, #\Newline should not cause input activation
  ;; if isearch is being performed.
  (when (and (or (activation-gesture-p gesture)
                 (climi::gesture-match gesture *completion-gestures*)
                 (climi::gesture-match gesture *help-gestures*)
                 (climi::gesture-match gesture *possibilities-gestures*))
             (directly-processing-p (drei-instance stream)))
    (end-of-buffer (point (drei-instance stream)))
    (unless (= (stream-scan-pointer stream)
               (size (buffer (drei-instance stream))))
      (queue-rescan stream))
    (setf (activation-gesture stream) gesture)
    (rescan-if-necessary stream)
    (return-from stream-process-gesture gesture))
  ;; XXX: The problem is that `*original-stream*' is a subclass of
  ;; DREI-INPUT-EDITING-MIXIN (`stream', actually) at this point,
  ;; which has an array as input buffer (as demanded by the spec),
  ;; while the `stream-unread-gesture' method expects an event queue.
  (let ((*original-stream* nil)
        (*standard-input* (encapsulating-stream-stream stream)))
    (when (proper-gesture-p gesture)
      (unread-gesture gesture))
    (read-gestures-and-act stream gesture type)))

(defmethod reset-scan-pointer ((stream drei-input-editing-mixin)
			       &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer)
  (setf (stream-rescanning stream) t))

;; This has been cribbed from SPLIT-SEQUENCE and lightly modified.
(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be
included in the result; otherwise they will be discarded.  All
other keywords work analogously to those for CL:SUBSTITUTE. The
second return value is an index suitable as an argument to
CL:SUBSEQ into the sequence indicating where processing stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (loop for left = start then (+ right 1)
       for right = (min (or (apply #'position delimiter seq 
                                   :start left
                                   other-keys)
                            len)
                        end)
       unless (and (= right left) 
                   remove-empty-subseqs) ; empty subseq we don't want
       if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
       return (values subseqs left)
       else
       collect (subseq seq left right) into subseqs
       and sum 1 into nr-elts
       until (>= right end)
       finally (return (values subseqs right)))))

(defmethod input-editor-format ((stream drei-input-editing-mixin)
				format-string
				&rest format-args)
  (let* ((drei (drei-instance stream))
         (output (apply #'format nil format-string format-args)))
    (when (or (stream-rescanning-p stream)
              (zerop (length output)))
      (return-from input-editor-format nil))
    ;; A noise string really should not contain a newline or Drei will
    ;; malfunction. Of course, the newlines inserted this way aren't
    ;; actually noise-strings. FIXME.
    (loop for (seq . rest) on (split-sequence #\Newline output)
       when (plusp (length seq))
       do (insert-object (point drei) (make-instance 'noise-string
                                                     :string seq))
       unless (null rest)
       do (insert-object (point drei) #\Newline))
    ;; Since everything inserted with this method is noise strings, we
    ;; do not bother to modify the scan pointer or queue rescans.
    (update-syntax (buffer drei) (syntax (buffer drei)))
    (display-drei-area drei)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; `Add-input-editor-command'
;;;
;;; The CLIM spec requires us to define a completely unusable function
;;; for mapping gestures to functions in the input editor. Since the
;;; CLIM spec does not define, or even suggest, any kind of
;;; programmatic access to the data structures of the input-editor for
;;; these function, it is utterly impossible to write portable
;;; input-editor functions using this
;;; facility. `Add-input-editor-command' is implemented like this in
;;; Drei: the specified gesture sequence is bound to the provided
;;; function in the `editor-table' command table, and will have a
;;; standard Drei command environment when invoked. This is sufficient
;;; for only the most trivial of commands, using `define-command' and
;;; `set-key' is a much, much more powerful mechanism, and it allows
;;; far more elegant handling of numeric arguments.

(defun add-input-editor-command (gestures function)
  "Set up Drei so performing `gestures' will result in the
invocation of `function' "
  (set-key function 'editor-table gestures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Presentation type specialization.

(define-presentation-method accept :around
  ((type expression)
   (stream drei-input-editing-mixin)
   view
   &key)
  (with-drei-options ((drei-instance stream)
                      :syntax "Lisp"
                      :keep-syntax t)
    (call-next-method)))

(define-presentation-method accept ((type expression)
                                    (stream drei-input-editing-mixin)
                                    (view textual-view)
				    &key)
  (let ((*completion-gestures* nil)
        (*possibilities-gestures* nil))
    (with-delimiter-gestures (nil :override t)
      (loop
         named control-loop
         with drei = (drei-instance stream)
         with syntax = (syntax (buffer drei))
         ;; The input context permits the user to mouse-select displayed
         ;; Lisp objects and put them into the input buffer as literal
         ;; objects.
         for gesture = (with-input-context ('expression :override nil)
                           (object type)
                           (read-gesture :stream stream)
                         (expression (performing-drei-operations (drei :with-undo t)
                                       (presentation-replace-input
                                        stream object type (view drei)
                                        :buffer-start (stream-insertion-pointer stream)
                                        :allow-other-keys t
                                        :accept-result nil))
                                     nil))
         ;; True if `gesture' was freshly read from the user, and not
         ;; just retrieved from the buffer during a rescan.
         for freshly-inserted = (not (equal (buffer-object
                                             (buffer drei)
                                             (1- (stream-scan-pointer stream)))
                                            gesture))
         for form = (drei-lisp-syntax::form-after syntax (input-position stream))
         ;; We do not stop until the input is complete and an activation
         ;; gesture has just been provided. The freshness check is so
         ;; #\Newline characters in the input will not cause premature
         ;; activation.
         until (and (activation-gesture-p gesture)
                    freshly-inserted
                    (drei-lisp-syntax::form-complete-p form))
         ;; We only want to process the gesture if it is fresh, because
         ;; if it isn't, it has already been processed at some point in
         ;; the past.
         when (and (activation-gesture-p gesture)
                   freshly-inserted)
         do (with-activation-gestures (nil :override t)
              (stream-process-gesture stream gesture nil))
         finally (unread-gesture gesture :stream stream)
         (let* ((object (drei-lisp-syntax::token-to-object syntax form
                                                           :read t
                                                           :package *package*))
                (ptype (presentation-type-of object)))
           (return-from control-loop
             (values object
                     (if (presentation-subtypep ptype 'expression)
                         ptype 'expression))))))))