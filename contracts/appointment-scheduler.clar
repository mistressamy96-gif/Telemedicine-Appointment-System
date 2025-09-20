;; appointment-scheduler
;; Time-slot booking and provider availability
;; No cross-contract calls or trait usage

(define-constant ERR-NOT-FOUND u500)
(define-constant ERR-NOT-AUTHORIZED u501)
(define-constant ERR-INVALID u502)
(define-constant ERR-CLOSED u503)
(define-constant ERR-FULL u504)

(define-data-var next-provider-id uint u1)
(define-data-var next-slot-id uint u1)
(define-data-var next-appointment-id uint u1)

;; providers: id -> { owner, name, specialty, active }
(define-map providers
  (tuple (provider-id uint))
  (tuple (owner principal)
         (name (string-ascii 64))
         (specialty (string-ascii 32))
         (active bool)))

;; slots: id -> { provider-id, day, start-min, end-min, capacity, booked, active }
(define-map slots
  (tuple (slot-id uint))
  (tuple (provider-id uint)
         (day uint)
         (start-min uint)
         (end-min uint)
         (capacity uint)
         (booked uint)
         (active bool)))

;; appointments: id -> { patient, slot-id, provider-id, canceled, notes }
(define-map appointments
  (tuple (appointment-id uint))
  (tuple (patient principal)
         (slot-id uint)
         (provider-id uint)
         (canceled bool)
         (notes (string-ascii 64))))

;; ============ helpers ============

(define-read-only (str-non-empty (s (string-ascii 128)))
  (ok (> (len s) u0)))

(define-read-only (slot-open? (slot-id uint))
  (match (map-get? slots { slot-id: slot-id }) s
    (ok (and (get active s) (< (get booked s) (get capacity s))))
    (ok false)))

(define-read-only (owns-provider? (provider-id uint) (who principal))
  (match (map-get? providers { provider-id: provider-id }) p
    (ok (is-eq (get owner p) who))
    (ok false)))

;; ============ public api ============

(define-public (register-provider (name (string-ascii 64)) (specialty (string-ascii 32)))
  (begin
    (if (or (not (unwrap! (str-non-empty name) (err ERR-INVALID)))
            (not (unwrap! (str-non-empty specialty) (err ERR-INVALID))))
        (err ERR-INVALID)
        (let ((id (var-get next-provider-id)))
          (var-set next-provider-id (+ id u1))
          (map-insert providers { provider-id: id }
            { owner: tx-sender, name: name, specialty: specialty, active: true })
          (ok id)))))

(define-public (deactivate-provider (provider-id uint))
  (match (map-get? providers { provider-id: provider-id }) p
    (if (is-eq (get owner p) tx-sender)
        (begin
          (map-set providers { provider-id: provider-id }
            { owner: (get owner p),
              name: (get name p),
              specialty: (get specialty p),
              active: false })
          (ok true))
        (err ERR-NOT-AUTHORIZED))
    (err ERR-NOT-FOUND)))

(define-public (open-slot (provider-id uint) (day uint) (start-min uint) (end-min uint) (capacity uint))
  (match (map-get? providers { provider-id: provider-id }) p
    (let ((owner (get owner p)) (active (get active p)))
      (if (not (is-eq owner tx-sender))
          (err ERR-NOT-AUTHORIZED)
          (if (or (not active) (or (>= start-min end-min) (is-eq capacity u0)))
              (err ERR-INVALID)
              (let ((id (var-get next-slot-id)))
                (var-set next-slot-id (+ id u1))
                (map-insert slots { slot-id: id }
                  { provider-id: provider-id,
                    day: day,
                    start-min: start-min,
                    end-min: end-min,
                    capacity: capacity,
                    booked: u0,
                    active: true })
                (ok id)))))
    (err ERR-NOT-FOUND)))

(define-public (close-slot (slot-id uint))
  (match (map-get? slots { slot-id: slot-id }) s
    (let ((pid (get provider-id s)))
      (if (not (unwrap! (owns-provider? pid tx-sender) (err ERR-NOT-AUTHORIZED)))
          (err ERR-NOT-AUTHORIZED)
          (begin
            (map-set slots { slot-id: slot-id }
              { provider-id: pid,
                day: (get day s),
                start-min: (get start-min s),
                end-min: (get end-min s),
                capacity: (get capacity s),
                booked: (get booked s),
                active: false })
            (ok true))))
    (err ERR-NOT-FOUND)))

(define-public (reopen-slot (slot-id uint))
  (match (map-get? slots { slot-id: slot-id }) s
    (let ((pid (get provider-id s)))
      (if (not (unwrap! (owns-provider? pid tx-sender) (err ERR-NOT-AUTHORIZED)))
          (err ERR-NOT-AUTHORIZED)
          (begin
            (map-set slots { slot-id: slot-id }
              { provider-id: pid,
                day: (get day s),
                start-min: (get start-min s),
                end-min: (get end-min s),
                capacity: (get capacity s),
                booked: (get booked s),
                active: true })
            (ok true))))
    (err ERR-NOT-FOUND)))

(define-public (book (slot-id uint) (notes (string-ascii 64)))
  (match (map-get? slots { slot-id: slot-id }) s
    (begin
      (if (not (get active s))
          (err ERR-CLOSED)
          (if (>= (get booked s) (get capacity s))
              (err ERR-FULL)
              (let ((aid (var-get next-appointment-id)))
                (var-set next-appointment-id (+ aid u1))
                (map-insert appointments { appointment-id: aid }
                  { patient: tx-sender,
                    slot-id: slot-id,
                    provider-id: (get provider-id s),
                    canceled: false,
                    notes: notes })
                (map-set slots { slot-id: slot-id }
                  { provider-id: (get provider-id s),
                    day: (get day s),
                    start-min: (get start-min s),
                    end-min: (get end-min s),
                    capacity: (get capacity s),
                    booked: (+ (get booked s) u1),
                    active: (get active s) })
                (ok aid)))))
    (err ERR-NOT-FOUND)))

(define-public (cancel-appointment (appointment-id uint))
  (match (map-get? appointments { appointment-id: appointment-id }) a
    (if (not (is-eq (get patient a) tx-sender))
        (err ERR-NOT-AUTHORIZED)
        (if (get canceled a)
            (ok false)
            (begin
              (map-set appointments { appointment-id: appointment-id }
                { patient: (get patient a),
                  slot-id: (get slot-id a),
                  provider-id: (get provider-id a),
                  canceled: true,
                  notes: (get notes a) })
              (match (map-get? slots { slot-id: (get slot-id a) }) s
                (let ((b (get booked s)))
                  (map-set slots { slot-id: (get slot-id a) }
                    { provider-id: (get provider-id s),
                      day: (get day s),
                      start-min: (get start-min s),
                      end-min: (get end-min s),
                      capacity: (get capacity s),
                      booked: (if (> b u0) (- b u1) u0),
                      active: (get active s) })
                  (ok true))
                (err ERR-NOT-FOUND)))))
    (err ERR-NOT-FOUND)))

;; ============ read-only ============

(define-read-only (get-provider (provider-id uint))
  (match (map-get? providers { provider-id: provider-id }) p
    (ok p)
    (err ERR-NOT-FOUND)))

(define-read-only (get-slot (slot-id uint))
  (match (map-get? slots { slot-id: slot-id }) s
    (ok s)
    (err ERR-NOT-FOUND)))

(define-read-only (get-appointment (appointment-id uint))
  (match (map-get? appointments { appointment-id: appointment-id }) a
    (ok a)
    (err ERR-NOT-FOUND)))

(define-read-only (slot-availability (slot-id uint))
  (match (map-get? slots { slot-id: slot-id }) s
    (ok (if (> (get capacity s) (get booked s)) (- (get capacity s) (get booked s)) u0))
    (err ERR-NOT-FOUND)))
