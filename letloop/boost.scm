;; Copyright © 2023 Amirouche A. BOUBEKKI <amirouche at hyper dev>
(library (letloop boost)

  (export make-boost
          boost-set
          boost-ref
          boost-delete
          boost-length
          boost-start
          boost-end
          boost-search
          boost-next
          boost-previous
          boost-key
          boost-value
          boost-fold
          boost-bytes
          boost-empty?
          boost-null?
          call-with-boost

          boost->alist

          ~check-boost-001
          ~check-boost-002
          ~check-boost-003
          ~check-boost-004
          ~check-boost-005
          ~check-boost-006
          ~check-boost-007
          ~check-boost-008
          ~check-boost-009
          ~check-boost-009-bis
          ~check-boost-009-ter
          ~check-boost-011
          ~check-boost-012
          ~check-boost-013
          ~check-boost-014/random
          ~check-boost-015/random
          ~check-boost-016/random
          ~check-boost-017/random
          )
  (import (chezscheme))

  (define-syntax define-record-type*
    (lambda (stx)
      (syntax-case stx ()
        ((_ <type>
            uid
            (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          (nongenerative uid)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...))))

        ((_ <type> (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          ;; XXX: The following expression sets the
                          ;; record type unique identifier, hence it
                          ;; is preferable that all record types have
                          ;; a different <type>. It is a good idea I
                          ;; can't forsee cases where <type> must be
                          ;; <foobar> instead of <letloop-foobar>,
                          ;; except that record instances are
                          ;; representation in the REPL are slightly
                          ;; longer, and given the uid is not random
                          ;; it is also more readable.
                          (nongenerative <type>)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...)))))))
  
  (define-record-type* <boost>
    (make-boost* key key-bytes value value-bytes length left right stack)
    boost?
    (key boost-key)
    (key-bytes boost-key-bytes)
    (value boost-value)
    (value-bytes boost-value-bytes)
    (length boost-length)
    (left boost-left)
    (right boost-right)
    ;; Keep around the top-down path inside stack to backtrack.
    (stack boost-stack))

  (define boost-null (make-boost* (bytevector) 0 (bytevector) 0 0 #f #f '()))

  (define (make-boost)
    boost-null)

  (define boost-null?
    (lambda (boost)
      (eq? boost boost-null)))

  (define boost-empty?
    (lambda (boost)
      (= (boost-length boost) 0)))

  (define boost-bytes
    (lambda (boost)
      (+ (boost-key-bytes boost)
         (boost-value-bytes boost))))

  (define bit-length fxlength)

  (define boost<?
    (lambda (a b)
      (fx<? (bit-length a) (bit-length b))))

  (define too-big?
    (lambda (a b)
      (boost<? a (fxarithmetic-shift-right b 1))))

  (define boost-join
    (lambda (key value left right)
      (make-boost* key
                   (+ (bytevector-length key)
                      (boost-key-bytes left)
                      (boost-key-bytes right))
                   value
                   (+ (bytevector-length value)
                      (boost-value-bytes left)
                      (boost-value-bytes right))
                   (fx+ (boost-length left)
                        (boost-length right)
                        1)
                   left
                   right
                   '())))

  (define boost-single-left-rotation
    (lambda (key value left right)
      (boost-join (boost-key right)
                  (boost-value right)
                  (boost-join key value left (boost-left right))
                  (boost-right right))))

  (define boost-double-left-rotation
    (lambda (key value left right)
      (boost-join (boost-key (boost-left right))
                  (boost-value (boost-left right))
                  (boost-join key
                              value
                              left
                              (boost-left (boost-left right)))
                  (boost-join (boost-key right)
                              (boost-value right)
                              (boost-right (boost-left right))
                              (boost-right right)))))

  (define boost-single-right-rotation
    (lambda (key value left right)
      (boost-join (boost-key left)
                  (boost-value left)
                  (boost-left left)
                  (boost-join key
                              value
                              (boost-right left)
                              right))))

  (define boost-double-right-rotation
    (lambda (key value left right)
      (boost-join (boost-key (boost-right left))
                  (boost-value (boost-right left))
                  (boost-join (boost-key left)
                              (boost-value left)
                              (boost-left left)
                              (boost-left (boost-right left)))
                  (boost-join key
                              value
                              (boost-right (boost-right left))
                              right))))


  (define boost-rebalance
    (lambda (key value left right)
      (if (too-big? (boost-length left) (boost-length right))
          (if (not (boost<? (boost-length (boost-right right))
                            (boost-length (boost-left right))))
              (boost-single-left-rotation key value left right)
              (boost-double-left-rotation key value left right))
          (if (too-big? (boost-length right) (boost-length left))
              (if (not (boost<? (boost-length (boost-left left))
                                (boost-length (boost-right left))))
                  (boost-single-right-rotation key value left right)
                  (boost-double-right-rotation key value left right))
              ;; otherwise join both trees with a top level node
              (boost-join key value left right)))))

  (define (bytevector-compare bytevector other)
    ;; Returns the symbol 'smaller if BYTEVECTOR is before OTHER,
    ;; returns the bytevector 'equal if they are equal and otherwise
    ;; returns 'bigger
    (let ((end (fxmin (bytevector-length bytevector)
                      (bytevector-length other))))
      (let loop ((index 0))
        (if (fx=? end index)
            ;; BYTEVECTOR and OTHER are equal until index; BYTEVECTOR
            ;; is smaller lexicographically, if it is smaller in
            ;; length.
            (if (fx=? (bytevector-length bytevector)
                      (bytevector-length other))
                'equal
                (if (fx<? (bytevector-length bytevector)
                          (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fxzero? delta)
                  (loop (fx+ 1 index))
                  (if (fxnegative? delta)
                      'smaller
                      'bigger)))))))

  (define boost-set
    (lambda (boost key value)
      ;; XXX: the empty bytevector aka. #vu8 is forbidden as key, guess why.
      ;;(pk 'boost-set boost key value)
      (if (boost-null? boost)
          (make-boost* key
                       (bytevector-length key)
                       value
                       (bytevector-length value)
                       1
                       boost-null
                       boost-null
                       '())
          (case (bytevector-compare key (boost-key boost))
            ((smaller) (boost-rebalance (boost-key boost)
                                        (boost-value boost)
                                        (boost-set (boost-left boost) key value)
                                        (boost-right boost)))
            ((equal) (boost-join key value (boost-left boost) (boost-right boost)))
            ((bigger) (boost-rebalance (boost-key boost)
                                       (boost-value boost)
                                       (boost-left boost)
                                       (boost-set (boost-right boost) key value)))
            (else (error 'boost "Unexpected bytevector-compare return value"))))))

  (define boost-balanced?
    (lambda (boost)
      (if (boost-null? boost)
          #t
          (and (not (too-big? (boost-length (boost-left boost)) (boost-length (boost-right boost))))
               (not (too-big? (boost-length (boost-right boost)) (boost-length (boost-left boost))))
               (boost-balanced? (boost-left boost))
               (boost-balanced? (boost-right boost))))))

  (define boost-start
    (lambda (boost)
      (let loop ((boost boost)
                 (stack (boost-stack boost)))
        (if (not (boost-null? (boost-left boost)))
            (loop (boost-left boost)
                  (cons boost stack))
            (make-boost* (boost-key boost)
                         (boost-key-bytes boost)
                         (boost-value boost)
                         (boost-value-bytes boost)
                         (boost-length boost)
                         (boost-left boost)
                         (boost-right boost)
                         stack)))))

  (define boost-end
    (lambda (boost)
      (let loop ((boost boost)
                 (stack (boost-stack boost)))
        (if (not (boost-null? (boost-right boost)))
            (loop (boost-right boost)
                  (cons boost stack))
            (make-boost* (boost-key boost)
                         (boost-key-bytes boost)
                         (boost-value boost)
                         (boost-value-bytes boost)
                         (boost-length boost)
                         (boost-left boost)
                         (boost-right boost)
                         stack)))))

  (define boost-next
    (lambda (boost)
      (if (boost-null? (boost-right boost))
          (let loop ((boost boost)
                     (stack (boost-stack boost)))
            (if (null? stack)
                #f
                (let ((parent (car stack)))
                  (if (bytevector=? (boost-key (boost-left parent)) (boost-key boost))
                      (make-boost* (boost-key parent)
                                   (boost-key-bytes parent)
                                   (boost-value parent)
                                   (boost-value-bytes parent)
                                   (boost-length parent)
                                   (boost-left parent)
                                   (boost-right parent)
                                   (cdr stack))
                      (loop (car stack) (cdr stack))))))
          (let loop ((boost (boost-right boost))
                     (stack (cons boost (boost-stack boost))))
            (if (boost-null? (boost-left boost))
                (make-boost* (boost-key boost)
                             (boost-key-bytes boost)
                             (boost-value boost)
                             (boost-value-bytes boost)
                             (boost-length boost)
                             (boost-left boost)
                             (boost-right boost)
                             stack)
                (loop (boost-left boost) (cons boost stack)))))))

  (define boost-previous
    (lambda (boost)
      (if (boost-null? (boost-left boost))
          (let loop ((boost boost)
                     (stack (boost-stack boost)))
            (if (null? stack)
                #f
                (let ((parent (car stack)))
                  (if (bytevector=? (boost-key (boost-right parent)) (boost-key boost))
                      (make-boost* (boost-key parent)
                                   (boost-key-bytes parent)
                                   (boost-value parent)
                                   (boost-value-bytes parent)
                                   (boost-length parent)
                                   (boost-left parent)
                                   (boost-right parent)
                                   (cdr stack))
                      (loop (car stack) (cdr stack))))))
          (let loop ((boost (boost-left boost))
                     (stack (cons boost (boost-stack boost))))
            (if (boost-null? (boost-right boost))
                (make-boost* (boost-key boost)
                             (boost-key-bytes boost)
                             (boost-value boost)
                             (boost-value-bytes boost)
                             (boost-length boost)
                             (boost-left boost)
                             (boost-right boost)
                             stack)
                (loop (boost-right boost) (cons boost stack)))))))

  (define boost-fold
    (lambda (proc out boost)
      (if (boost-empty? boost)
          out
          (let ((out (proc (boost-key boost)
                           (boost-value boost)
                           out))
                (next (boost-next boost)))
            (if next
                (boost-fold proc out next)
                out)))))

  (define boost->alist
    (lambda (boost)
      (let loop ((boost (boost-end boost))
                 (out '()))
        (if (not boost)
            out
            (loop (boost-previous boost) (cons (cons (boost-key boost) (boost-value boost)) out))))))

  (define (make-boost** boost stack)
    (make-boost* (boost-key boost)
                 (boost-key-bytes boost)
                 (boost-value boost)
                 (boost-value-bytes boost)
                 (boost-length boost)
                 (boost-left boost)
                 (boost-right boost)
                 stack))

  (define boost-search
    (lambda (boost key)
      ;; Can search a boost that was already moved...
      (if (boost-null? boost)
          (values #f boost-null)
          (let loop ((boost boost)
                     (stack '()))
            (case (bytevector-compare key (boost-key boost))
              ((smaller)
               (if (boost-null? (boost-left boost))
                   (values 'smaller (make-boost** boost stack))
                   (loop (boost-left boost) (cons boost stack))))
              ((bigger)
               (if (boost-null? (boost-right boost))
                   (values 'bigger (make-boost** boost stack))
                   (loop (boost-right boost) (cons boost stack))))
              (else (values 'equal (make-boost** boost stack))))))))

  (define call-with-boost
    ;; In the returned values swap boost and position.
    ;;
    ;; TODO: Remove boost-search, replace with it this procedure.
    (lambda (boost key proc)
      (call-with-values (lambda () (boost-search boost key))
        (lambda (position boost)
          (proc boost position)))))

  (define boost-ref
    (lambda (boost key)
      (call-with-values (lambda () (boost-search boost key))
        (lambda (position boost)
          (if (eq? position 'equal)
              boost
              #f)))))

  (define boost-delete
    (lambda (boost key)

      (define boost-delete-min
        (lambda (boost)
          (if (boost-null? (boost-left boost))
              (boost-right boost)
              (boost-rebalance (boost-key boost-key)
                               (boost-value boost-value)
                               (boost-delete-min (boost-left boost))
                               (boost-right boost)))))

      (define boost-concat3
        (lambda (key value left right)
          (if (boost-null? (boost-left left))
              (boost-set right key value)
              (if (boost-null? (boost-right left))
                  (boost-set left key value)
                  (cond
                   ((too-big? (boost-length left) (boost-length right))
                    (boost-rebalance (boost-key right)
                                     (boost-value right)
                                     left
                                     (boost-concat3 key
                                                    value
                                                    (boost-left right)
                                                    (boost-right right))))
                   ((too-big? (boost-length right) (boost-length left))
                    (boost-rebalance (boost-key left)
                                     (boost-value left)
                                     (boost-concat3 key
                                                    value
                                                    (boost-left left)
                                                    (boost-right left))
                                     right))
                   (else (boost-rebalance key value left right)))))))

      (define boost-concat2
        (lambda (boost other)
          (if (boost-null? boost)
              other
              (if (boost-null? other)
                  boost
                  (call-with-values (lambda () (boost-search other (bytevector)))
                    (lambda (_ min)
                      (boost-concat3 (boost-key boost)
                                     (boost-value boost)
                                     boost
                                     (boost-delete-min other))))))))

      (if (boost-null? boost)
          #f
          (case (bytevector-compare key (boost-key boost))
            ((bigger) (boost-rebalance (boost-key boost)
                                       (boost-value boost)
                                       (boost-left boost)
                                       (boost-delete (boost-right boost) key)))
            ((smaller) (boost-rebalance (boost-key boost)
                                        (boost-value boost)
                                        (boost-delete (boost-left boost) key)
                                        (boost-right boost)))
            (else (boost-concat2 (boost-left boost) (boost-right boost)))))))

  (define-syntax check
    (syntax-rules ()
      ((check v)
       (let ((v* v))
         (when (or (eq? v* (void))
                   (not v*))
           (error 'check "Unexpected falsy or void object" 'v))))
      ((check a b)
       (let ((a* a)
             (b* b))
         (when (or (eq? a* (void))
                   (eq? b* (void))
                   (not (equal? a* b*)))
           (error 'check "void or unequal values" a* b*))))))

  (define pk
    (lambda args
      (write args)(newline)
      (flush-output-port)
      (car (reverse args))))
  
  (define ~check-boost-001
    (lambda ()
      (check (boost-null? boost-null))))

  (define ~check-boost-002
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101)))
             )
        (check (equal? (boost->alist boost)
                       `((,(bytevector 13) . ,(bytevector 13))
                         (,(bytevector 14) . ,(bytevector 14))
                         (,(bytevector 42) . ,(bytevector 42))
                         (,(bytevector 101) . ,(bytevector 101))))))))

  (define ~check-boost-003
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))
        (check (fx<? (bytevector-u8-ref (boost-value boost) 0) (bytevector-u8-ref (boost-value (boost-next boost)) 0))))))

  (define ~check-boost-004
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))
        (check (fx<? (bytevector-u8-ref (boost-value boost) 0) (bytevector-u8-ref (boost-value (boost-next boost)) 0))))))

  (define ~check-boost-005
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))
        (let* ((a (boost-value (boost-previous boost)))
               (b (boost-value (boost-next (boost-previous boost))))
               (c (boost-value (boost-previous (boost-next boost))))
               (d (boost-value (boost-next boost))))
          (check (apply fx<=? (map (lambda (x) (bytevector-u8-ref x 0)) (list a b c d))))))))

  (define ~check-boost-006
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))
        (check (bytevector=? (boost-value (boost-start boost)) (bytevector 13))))))

  (define ~check-boost-007
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))
        (check (bytevector=? (boost-value (boost-end boost)) (bytevector 101))))))

  ;; same as boost->alist but we start from the first key until the
  ;; end, and accumulate key-value pairs in a list, and return that
  ;; without the need to call reverse.
  (define boost->alist/reversed
    (lambda (boost)
      (let loop ((boost (boost-start boost))
                 (out '()))
        (if (not boost)
            out
            (loop (boost-next boost) (cons (cons (boost-key boost) (boost-value boost)) out))))))

  (define ~check-boost-008
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))

        (check (equal? (reverse (boost->alist/reversed boost)) (boost->alist boost))))))

  (define ~check-boost-009
    (lambda ()

      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))

        (let loop ((vs (list 13 14 42 101)))
          (unless (null? vs)
            (call-with-values (lambda () (boost-search boost (bytevector (car vs))))
              (lambda (position boost)
                (check (eq? position 'equal))
                (check (= (bytevector-u8-ref (boost-value boost) 0) (car vs)))
                (loop (cdr vs)))))))))

  (define ~check-boost-009-bis
    (lambda ()

      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 13 36) (bytevector 1)))
             (boost (boost-set boost (bytevector 13 38) (bytevector 2)))
             (key (bytevector 13 37)))

        (call-with-boost boost key
                         (lambda (boost position)
                           (check (eq? position
                                       (bytevector-compare key (boost-key boost)))))))))

  (define ~check-boost-009-ter
    (lambda ()

      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 13 38) (bytevector 2)))
             (boost (boost-set boost (bytevector 13 36) (bytevector 1)))
             (key (bytevector 13 37)))

        (call-with-boost boost key
                         (lambda (boost position)
                           (check (eq? position
                                       (bytevector-compare key (boost-key boost)))))))))

  (define ~check-boost-011
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))

        (let loop ((vs (list 42 13 14 101)))
          (unless (null? vs)
            (check (= (car vs) (bytevector-u8-ref (boost-value (boost-ref boost (bytevector (car vs)))) 0)))
            (loop (cdr vs)))))))

  (define ~check-boost-012
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))

        (let loop ((vs (list 41 0 7 9 255)))
          (unless (null? vs)
            (check (not (boost-ref boost (bytevector (car vs)))))
            (loop (cdr vs)))))))

  (define ~check-boost-013
    (lambda ()
      (let* ((boost (make-boost))
             (boost (boost-set boost (bytevector 42) (bytevector 42)))
             (boost (boost-set boost (bytevector 13) (bytevector 13)))
             (boost (boost-set boost (bytevector 14) (bytevector 14)))
             (boost (boost-set boost (bytevector 101) (bytevector 101))))

        (let ((new (boost-delete boost (bytevector 42))))
          (check (equal? (boost->alist new)
                         `((,(bytevector 13) . ,(bytevector 13)) (,(bytevector 14) . ,(bytevector 14)) (,(bytevector 101) . ,(bytevector 101)))))))))

  (define bytevector<?
    (lambda (a b)
      (case (bytevector-compare a b)
        ((smaller) #t)
        (else #f))))

  (define ~check-boost-014/random
    (lambda ()
      (let loop ((boost (make-boost))
                 (out '())
                 (count 16))
        (if (fxzero? count)
            (begin
              (check (equal? (boost->alist boost) (sort (lambda (a b) (bytevector<? (car a) (car b))) out))))
            (let ((key (make-bytevector 8))
                  (value (random (expt 2 64))))
              (bytevector-u64-set! key 0 value 'big)
              (loop (boost-set boost key key) (cons (cons key key) out) (fx- count 1)))))))

  (define ~check-boost-015/random
    (lambda ()
      (let loop ((boost (make-boost))
                 (out '())
                 (count 16))
        (if (fxzero? count)
            (begin
              (check (equal? (boost->alist/reversed boost) (reverse (sort (lambda (a b) (bytevector<? (car a) (car b))) out)))))
            (let ((key (make-bytevector 8))
                  (value (random (expt 2 64))))
              (bytevector-u64-set! key 0 value 'big)
              (loop (boost-set boost key key) (cons (cons key key) out) (fx- count 1)))))))

  (define random-bytevector
    (lambda (length)
      (define bytevector (make-bytevector (fx+ (random length) 1)))
      (let loop ((length (bytevector-length bytevector)))
        (unless (fxzero? length)
          (bytevector-u8-set! bytevector (fx- length 1) (random 256))
          (loop (fx- length 1))))
      bytevector))

  (define ~check-boost-016/random
    (lambda ()
      (define magic 2048)
      (let loop ((boost (make-boost))
                 (count 2048)
                 (bytes 0))
        (if (fxzero? count)
            (begin
              (check (= (boost-length boost) 2048))
              (check (= (boost-bytes boost) bytes)))
            (let ((key (random-bytevector 2048))
                  (value (random-bytevector 2048)))
              (loop (boost-set boost key value)
                    (fx- count 1)
                    (fx+ (bytevector-length key)
                         (bytevector-length value)
                         bytes)))))))

  (define ~check-boost-017/random
    (lambda ()
      (define magic 2048)
      (define length (random magic))
      (let loop ((boost (make-boost))
                 (count length))
        (if (fxzero? count)
            (check (= (boost-length boost) length))
            (let ((key (random-bytevector magic))
                  (value (random-bytevector magic)))
              (loop (boost-set boost key value)
                    (fx- count 1)))))))

  )
