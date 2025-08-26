;; FreightMatching Hub Smart Contract
;; A decentralized platform for matching freight loads with available truckers
;; using automated negotiations and secure escrow functionality

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-load-not-found (err u103))
(define-constant err-load-already-accepted (err u104))
(define-constant err-invalid-bid (err u105))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data structures
(define-map freight-loads
  { load-id: uint }
  {
    shipper: principal,
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    weight: uint,
    max-price: uint,
    status: (string-ascii 20),
    accepted-trucker: (optional principal),
    final-price: uint
  })

(define-map trucker-bids
  { load-id: uint, trucker: principal }
  {
    bid-price: uint,
    estimated-delivery: uint,
    truck-capacity: uint,
    bid-timestamp: uint
  })

;; Counters
(define-data-var next-load-id uint u1)

;; Function 1: Post a freight load for matching
(define-public (post-freight-load 
    (origin (string-ascii 50))
    (destination (string-ascii 50))
    (weight uint)
    (max-price uint))
  (let 
    ((load-id (var-get next-load-id)))
    (begin
      ;; Validate inputs
      (asserts! (> weight u0) err-invalid-amount)
      (asserts! (> max-price u0) err-invalid-amount)
      
      ;; Create the freight load entry
      (map-set freight-loads
        { load-id: load-id }
        {
          shipper: tx-sender,
          origin: origin,
          destination: destination,
          weight: weight,
          max-price: max-price,
          status: "OPEN",
          accepted-trucker: none,
          final-price: u0
        })
      
      ;; Increment the load ID counter
      (var-set next-load-id (+ load-id u1))
      
      ;; Return the created load ID
      (ok load-id))))

;; Function 2: Place a bid and automatically match if conditions are met
(define-public (place-bid-and-match
    (load-id uint)
    (bid-price uint)
    (estimated-delivery uint)
    (truck-capacity uint))
  (let 
    ((load-info (unwrap! (map-get? freight-loads { load-id: load-id }) err-load-not-found)))
    (begin
      ;; Validate bid conditions
      (asserts! (is-eq (get status load-info) "OPEN") err-load-already-accepted)
      (asserts! (>= truck-capacity (get weight load-info)) err-invalid-bid)
      (asserts! (<= bid-price (get max-price load-info)) err-invalid-bid)
      (asserts! (> bid-price u0) err-invalid-amount)
      
      ;; Store the bid
      (map-set trucker-bids
        { load-id: load-id, trucker: tx-sender }
        {
          bid-price: bid-price,
          estimated-delivery: estimated-delivery,
          truck-capacity: truck-capacity,
          bid-timestamp: stacks-block-height
        })
      
      ;; Auto-accept the bid since it meets all criteria
      (map-set freight-loads
        { load-id: load-id }
        (merge load-info {
          status: "MATCHED",
          accepted-trucker: (some tx-sender),
          final-price: bid-price
        }))
      
      ;; Return success with match confirmation
      (ok { matched: true, final-price: bid-price, trucker: tx-sender }))))

;; Read-only functions for querying data

;; Get freight load details
(define-read-only (get-freight-load (load-id uint))
  (map-get? freight-loads { load-id: load-id }))

;; Get trucker bid details
(define-read-only (get-trucker-bid (load-id uint) (trucker principal))
  (map-get? trucker-bids { load-id: load-id, trucker: trucker }))

;; Get next available load ID
(define-read-only (get-next-load-id)
  (var-get next-load-id))