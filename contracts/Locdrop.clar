(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_LOCATION (err u101))
(define-constant ERR_DROP_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_CLAIMED (err u103))
(define-constant ERR_DROP_EXPIRED (err u104))
(define-constant ERR_INSUFFICIENT_RADIUS (err u105))
(define-constant ERR_INVALID_COORDINATES (err u106))
(define-constant ERR_DROP_NOT_ACTIVE (err u107))
(define-constant ERR_INVALID_DURATION (err u108))
(define-constant ERR_TOKEN_TRANSFER_FAILED (err u109))

(define-non-fungible-token locdrop-nft uint)

(define-data-var next-drop-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var contract-paused bool false)

(define-map drops
  uint
  {
    creator: principal,
    latitude: int,
    longitude: int,
    radius: uint,
    token-uri: (string-ascii 256),
    reward-amount: uint,
    start-block: uint,
    end-block: uint,
    max-claims: uint,
    current-claims: uint,
    active: bool
  }
)

(define-map user-claims
  { drop-id: uint, user: principal }
  { claimed: bool, claim-block: uint, token-id: uint }
)

(define-map token-claims
  uint
  { drop-id: uint, user: principal }
)

(define-map user-locations
  principal
  { latitude: int, longitude: int, last-update: uint }
)

(define-map drop-participants
  uint
  { participant-count: uint }
)

(define-map authorized-oracles
  principal
  bool
)

(define-public (set-oracle-status (oracle principal) (authorized bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (map-set authorized-oracles oracle authorized))
  )
)

(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (var-set contract-paused (not (var-get contract-paused))))
  )
)

(define-public (create-drop 
  (latitude int) 
  (longitude int) 
  (radius uint) 
  (token-uri (string-ascii 256))
  (reward-amount uint)
  (duration uint)
  (max-claims uint))
  (let
    (
      (drop-id (var-get next-drop-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    (asserts! (> radius u0) ERR_INSUFFICIENT_RADIUS)
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    (asserts! (> max-claims u0) ERR_INVALID_LOCATION)
    
    (map-set drops drop-id {
      creator: tx-sender,
      latitude: latitude,
      longitude: longitude,
      radius: radius,
      token-uri: token-uri,
      reward-amount: reward-amount,
      start-block: current-block,
      end-block: end-block,
      max-claims: max-claims,
      current-claims: u0,
      active: true
    })
    
    (map-set drop-participants drop-id { participant-count: u0 })
    (var-set next-drop-id (+ drop-id u1))
    (ok drop-id)
  )
)

(define-public (update-location (latitude int) (longitude int))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    
    (map-set user-locations tx-sender {
      latitude: latitude,
      longitude: longitude,
      last-update: stacks-block-height
    })
    (ok true)
  )
)

(define-read-only (calculate-distance (lat1 int) (lon1 int) (lat2 int) (lon2 int))
  (let
    (
      (lat-diff (if (> lat1 lat2) (- lat1 lat2) (- lat2 lat1)))
      (lon-diff (if (> lon1 lon2) (- lon1 lon2) (- lon2 lon1)))
      (distance-squared (+ (* lat-diff lat-diff) (* lon-diff lon-diff)))
    )
    (sqrti distance-squared)
  )
)

(define-public (claim-drop (drop-id uint))
  (let
    (
      (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
      (user-location (unwrap! (map-get? user-locations tx-sender) ERR_INVALID_LOCATION))
      (current-block stacks-block-height)
      (token-id (var-get next-token-id))
      (distance (calculate-distance 
        (get latitude user-location) 
        (get longitude user-location)
        (get latitude drop-data) 
        (get longitude drop-data)))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (get active drop-data) ERR_DROP_NOT_ACTIVE)
    (asserts! (>= current-block (get start-block drop-data)) ERR_DROP_NOT_ACTIVE)
    (try! (nft-mint? locdrop-nft token-id tx-sender))
    
    (map-set user-claims 
      { drop-id: drop-id, user: tx-sender }
      { claimed: true, claim-block: current-block, token-id: token-id }
    )
    
    (map-set token-claims
      token-id
      { drop-id: drop-id, user: tx-sender }
    )
    
    (map-set user-claims 
      { drop-id: drop-id, user: tx-sender }
      { claimed: true, claim-block: current-block, token-id: token-id }
    )
    
    (map-set drops drop-id 
      (merge drop-data { current-claims: (+ (get current-claims drop-data) u1) })
    )
    
    (var-set next-token-id (+ token-id u1))
    (ok token-id)
  )
)

(define-public (deactivate-drop (drop-id uint))
  (let
    (
      (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator drop-data)) ERR_NOT_AUTHORIZED)
    (map-set drops drop-id (merge drop-data { active: false }))
    (ok true)
  )
)

(define-public (oracle-update-location (user principal) (latitude int) (longitude int))
  (begin
    (asserts! (default-to false (map-get? authorized-oracles tx-sender)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    
    (map-set user-locations user {
      latitude: latitude,
      longitude: longitude,
      last-update: stacks-block-height
    })
    (ok true)
  )
)

(define-read-only (get-drop (drop-id uint))
  (map-get? drops drop-id)
)

(define-read-only (get-user-location (user principal))
  (map-get? user-locations user)
)

(define-read-only (get-user-claim (drop-id uint) (user principal))
  (map-get? user-claims { drop-id: drop-id, user: user })
)

(define-read-only (get-next-drop-id)
  (var-get next-drop-id)
)
(define-read-only (get-token-uri (token-id uint))
  (let
    (
      (claim-info (unwrap! (map-get? token-claims token-id) (err "Token not found")))
      (drop-data (unwrap! (map-get? drops (get drop-id claim-info)) (err "Drop not found")))
    )
    (ok (some (get token-uri drop-data)))
  )
)

;; (define-read-only (get-token-uri (token-id uint))
;;   (let
;;     (
;;     ;;   (claim-info (unwrap! (get-claim-by-token-id token-id) (err "Token not found")))
;;       (drop-data (unwrap! (map-get? drops (get drop-id claim-info)) (err "Drop not found")))
;;     )
;;     (ok (some (get token-uri drop-data)))
;;   )
;; )



;; (define-read-only (is-user-in-range (user principal) (drop-id uint))
;;   (let
;;     (
;;       (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
;;       (user-location (unwrap! (map-get? user-locations user) ERR_INVALID_LOCATION))
;;       (distance (calculate-distance 
;;         (get latitude user-location) 
;;         (get longitude user-location)
;;         (get latitude drop-data) 
;;         (get longitude drop-data)))
;;     )
;;     ;; (ok (<= distance (get radius drop-data)))
;;   )
;; )

(define-read-only (get-active-drops)
  (ok (var-get next-drop-id))
)

(define-read-only (is-drop-claimable (drop-id uint) (user principal))
  (let
    (
      (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
      (current-block stacks-block-height)
      (user-claim (map-get? user-claims { drop-id: drop-id, user: user }))
    )
    (ok (and 
      (get active drop-data)
      (>= current-block (get start-block drop-data))
      (<= current-block (get end-block drop-data))
      (< (get current-claims drop-data) (get max-claims drop-data))
      (is-none user-claim)
    ))
  )
)

