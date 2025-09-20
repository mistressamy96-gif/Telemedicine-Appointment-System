;; e-prescription-service
;; Prescription generation and pharmacy integration (simplified)
;; No cross-contract calls or trait usage

(define-constant ERR-NOT-FOUND u600)
(define-constant ERR-NOT-AUTHORIZED u601)
(define-constant ERR-INVALID u602)
(define-constant ERR-CANCELED u603)
(define-constant ERR-NO-REFILLS u604)

(define-data-var next-rx-id uint u1)

;; prescriptions: rx-id -> { prescriber, patient, drug, dosage, quantity, refills, issued, canceled, filled-count }
(define-map prescriptions
  (tuple (rx-id uint))
  (tuple (prescriber principal)
         (patient principal)
         (drug (string-ascii 64))
         (dosage (string-ascii 32))
         (quantity uint)
         (refills uint)
         (issued bool)
         (canceled bool)
         (filled-count uint)))

;; fill log: (rx-id, index) -> { pharmacy, quantity }
(define-map fills
  (tuple (rx-id uint) (index uint))
  (tuple (pharmacy principal)
         (quantity uint)))

;; ===== helpers =====

(define-read-only (str-non-empty (s (string-ascii 128)))
  (ok (> (len s) u0)))

(define-read-only (max-fills (refills uint))
  (ok (+ refills u1)))

;; ===== public api =====

(define-public (issue (patient principal) (drug (string-ascii 64)) (dosage (string-ascii 32)) (quantity uint) (refills uint))
  (begin
    (if (or (not (unwrap! (str-non-empty drug) (err ERR-INVALID)))
            (not (unwrap! (str-non-empty dosage) (err ERR-INVALID)))
            (is-eq quantity u0))
        (err ERR-INVALID)
        (let ((id (var-get next-rx-id)))
          (var-set next-rx-id (+ id u1))
          (map-insert prescriptions { rx-id: id }
            { prescriber: tx-sender,
              patient: patient,
              drug: drug,
              dosage: dosage,
              quantity: quantity,
              refills: refills,
              issued: true,
              canceled: false,
              filled-count: u0 })
          (ok id)))))

(define-public (cancel (rx-id uint))
  (match (map-get? prescriptions { rx-id: rx-id }) rx
    (if (not (is-eq (get prescriber rx) tx-sender))
        (err ERR-NOT-AUTHORIZED)
        (if (get canceled rx)
            (ok false)
            (begin
              (map-set prescriptions { rx-id: rx-id }
                { prescriber: (get prescriber rx),
                  patient: (get patient rx),
                  drug: (get drug rx),
                  dosage: (get dosage rx),
                  quantity: (get quantity rx),
                  refills: (get refills rx),
                  issued: (get issued rx),
                  canceled: true,
                  filled-count: (get filled-count rx) })
              (ok true))))
    (err ERR-NOT-FOUND)))

(define-public (fill (rx-id uint) (qty uint))
  (match (map-get? prescriptions { rx-id: rx-id }) rx
    (let ((canceled (get canceled rx))
          (count (get filled-count rx))
          (max (unwrap! (max-fills (get refills rx)) (err ERR-INVALID)))
          (base (get quantity rx)))
      (if canceled
          (err ERR-CANCELED)
          (if (or (is-eq qty u0) (not (is-eq qty base)))
              (err ERR-INVALID)
              (if (>= count max)
                  (err ERR-NO-REFILLS)
                  (let ((next (+ count u1)))
                    (map-insert fills { rx-id: rx-id, index: next }
                      { pharmacy: tx-sender, quantity: qty })
                    (map-set prescriptions { rx-id: rx-id }
                      { prescriber: (get prescriber rx),
                        patient: (get patient rx),
                        drug: (get drug rx),
                        dosage: (get dosage rx),
                        quantity: base,
                        refills: (get refills rx),
                        issued: (get issued rx),
                        canceled: false,
                        filled-count: next })
                    (ok next))))))
    (err ERR-NOT-FOUND)))

;; ===== read-only =====

(define-read-only (get-rx (rx-id uint))
  (match (map-get? prescriptions { rx-id: rx-id }) rx
    (ok rx)
    (err ERR-NOT-FOUND)))

(define-read-only (get-fill (rx-id uint) (index uint))
  (match (map-get? fills { rx-id: rx-id, index: index }) f
    (ok f)
    (err ERR-NOT-FOUND)))

(define-read-only (remaining-fills (rx-id uint))
  (match (map-get? prescriptions { rx-id: rx-id }) rx
    (let ((max (unwrap! (max-fills (get refills rx)) (err ERR-INVALID)))
          (count (get filled-count rx)))
      (ok (if (> max count) (- max count) u0)))
    (err ERR-NOT-FOUND)))
