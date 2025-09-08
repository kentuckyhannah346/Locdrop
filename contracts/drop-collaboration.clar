;; Drop Collaboration System - pool resources for joint GPS drops
(define-constant ERR_NOT_FOUND (err u200))
(define-constant ERR_NOT_AUTHORIZED (err u201))
(define-constant ERR_ALREADY_CONTRIBUTED (err u202))
(define-constant ERR_ALREADY_VOTED (err u203))
(define-constant ERR_PROPOSAL_EXPIRED (err u204))
(define-constant ERR_PROPOSAL_NOT_READY (err u205))
(define-constant ERR_ALREADY_FINALIZED (err u206))
(define-constant ERR_INVALID_AMOUNT (err u207))
(define-constant ERR_INVALID_COORDINATES (err u208))
(define-constant ERR_TRANSFER_FAILED (err u209))
(define-data-var next-proposal-id uint u1)
(define-map collab-proposals uint {
  proposer: principal, latitude: int, longitude: int, radius: uint,
  token-uri: (string-ascii 256), reward-target: uint, deadline: uint,
  finalized: bool, locdrop-drop-id: (optional uint), duration: uint, max-claims: uint
})
(define-map contributors { proposal-id: uint, contributor: principal }
  { stx-contributed: uint, vote: (optional bool) })
(define-map proposal-stats uint {
  total-contributed: uint, yes-votes: uint, no-votes: uint, contributor-count: uint
})
(define-public (propose-collab 
  (latitude int) 
  (longitude int) 
  (radius uint) 
  (token-uri (string-ascii 256))
  (reward-target uint)
  (deadline uint)
  (duration uint)
  (max-claims uint)
  (initial-contribution uint))
  (let
    (
      (proposal-id (var-get next-proposal-id))
    )
    (asserts! (> initial-contribution u0) ERR_INVALID_AMOUNT)
    (asserts! (> reward-target u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline stacks-block-height) ERR_PROPOSAL_EXPIRED)
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    (asserts! (> radius u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration u0) ERR_INVALID_AMOUNT)
    (asserts! (> max-claims u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? initial-contribution tx-sender (as-contract tx-sender)))
    (map-set collab-proposals proposal-id {
      proposer: tx-sender, latitude: latitude, longitude: longitude, radius: radius,
      token-uri: token-uri, reward-target: reward-target, deadline: deadline,
      finalized: false, locdrop-drop-id: none, duration: duration, max-claims: max-claims
    })
    (map-set contributors { proposal-id: proposal-id, contributor: tx-sender }
      { stx-contributed: initial-contribution, vote: none })
    (map-set proposal-stats proposal-id {
      total-contributed: initial-contribution, yes-votes: u0,
      no-votes: u0, contributor-count: u1
    })
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (join-collab (proposal-id uint) (amount uint))
  (let ((proposal (unwrap! (map-get? collab-proposals proposal-id) ERR_NOT_FOUND))
        (existing-contributor (map-get? contributors { proposal-id: proposal-id, contributor: tx-sender }))
        (stats (unwrap! (map-get? proposal-stats proposal-id) ERR_NOT_FOUND)))
    (asserts! (< stacks-block-height (get deadline proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (not (get finalized proposal)) ERR_ALREADY_FINALIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none existing-contributor) ERR_ALREADY_CONTRIBUTED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set contributors { proposal-id: proposal-id, contributor: tx-sender }
      { stx-contributed: amount, vote: none })
    (map-set proposal-stats proposal-id {
      total-contributed: (+ (get total-contributed stats) amount),
      yes-votes: (get yes-votes stats), no-votes: (get no-votes stats),
      contributor-count: (+ (get contributor-count stats) u1)
    })
    (ok true)
  )
)

(define-public (vote-collab (proposal-id uint) (approve bool))
  (let ((proposal (unwrap! (map-get? collab-proposals proposal-id) ERR_NOT_FOUND))
        (contributor-data (unwrap! (map-get? contributors { proposal-id: proposal-id, contributor: tx-sender }) ERR_NOT_AUTHORIZED))
        (stats (unwrap! (map-get? proposal-stats proposal-id) ERR_NOT_FOUND)))
    (asserts! (< stacks-block-height (get deadline proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (not (get finalized proposal)) ERR_ALREADY_FINALIZED)
    (asserts! (is-none (get vote contributor-data)) ERR_ALREADY_VOTED)
    (map-set contributors { proposal-id: proposal-id, contributor: tx-sender }
      (merge contributor-data { vote: (some approve) }))
    (map-set proposal-stats proposal-id (if approve
        (merge stats { yes-votes: (+ (get yes-votes stats) u1) })
        (merge stats { no-votes: (+ (get no-votes stats) u1) })))
    (ok true)
  )
)
(define-public (finalize-collab (proposal-id uint))
  (let ((proposal (unwrap! (map-get? collab-proposals proposal-id) ERR_NOT_FOUND))
        (stats (unwrap! (map-get? proposal-stats proposal-id) ERR_NOT_FOUND))
        (majority-threshold (/ (get contributor-count stats) u2)))
    (asserts! (>= stacks-block-height (get deadline proposal)) ERR_PROPOSAL_NOT_READY)
    (asserts! (not (get finalized proposal)) ERR_ALREADY_FINALIZED)
    (asserts! (>= (get total-contributed stats) (get reward-target proposal)) ERR_PROPOSAL_NOT_READY)
    (asserts! (> (get yes-votes stats) majority-threshold) ERR_PROPOSAL_NOT_READY)
    (match (as-contract (contract-call? .Locdrop create-drop
      (get latitude proposal) (get longitude proposal) (get radius proposal)
      (get token-uri proposal) (get total-contributed stats) (get duration proposal)
      (get max-claims proposal) false))
      success-drop-id (begin
        (map-set collab-proposals proposal-id
          (merge proposal { finalized: true, locdrop-drop-id: (some success-drop-id) }))
        (ok success-drop-id))
      error (err u500)
    )
  )
)
(define-public (withdraw-contribution (proposal-id uint))
  (let ((proposal (unwrap! (map-get? collab-proposals proposal-id) ERR_NOT_FOUND))
        (contributor-data (unwrap! (map-get? contributors { proposal-id: proposal-id, contributor: tx-sender }) ERR_NOT_AUTHORIZED))
        (stats (unwrap! (map-get? proposal-stats proposal-id) ERR_NOT_FOUND))
        (majority-threshold (/ (get contributor-count stats) u2)))
    (asserts! (>= stacks-block-height (get deadline proposal)) ERR_PROPOSAL_NOT_READY)
    (asserts! (not (get finalized proposal)) ERR_ALREADY_FINALIZED)
    (asserts! (or (< (get total-contributed stats) (get reward-target proposal))
      (<= (get yes-votes stats) majority-threshold)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get stx-contributed contributor-data) tx-sender tx-sender)))
    (map-delete contributors { proposal-id: proposal-id, contributor: tx-sender })
    (ok (get stx-contributed contributor-data))
  )
)
(define-read-only (get-proposal (proposal-id uint))
  (map-get? collab-proposals proposal-id))
(define-read-only (get-contributor (proposal-id uint) (contributor principal))
  (map-get? contributors { proposal-id: proposal-id, contributor: contributor }))
(define-read-only (get-proposal-stats (proposal-id uint))
  (map-get? proposal-stats proposal-id))
(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id))
