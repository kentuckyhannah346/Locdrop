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
(define-constant ERR_ACHIEVEMENT_NOT_FOUND (err u110))
(define-constant ERR_SEASON_NOT_ACTIVE (err u111))
(define-constant ERR_INVALID_SEASON (err u112))
(define-constant ERR_ALREADY_CLAIMED_REWARD (err u113))
(define-constant ERR_NOT_VERIFIED (err u114))
(define-constant ERR_ALREADY_VERIFIED (err u115))
(define-constant ERR_VERIFICATION_EXPIRED (err u116))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u117))
(define-constant ERR_VERIFIER_NOT_REGISTERED (err u118))
(define-constant ERR_CANNOT_VERIFY_OWN_DROP (err u119))

(define-non-fungible-token locdrop-nft uint)

(define-data-var next-drop-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var current-season uint u1)
(define-data-var season-start-block uint u0)
(define-data-var season-end-block uint u0)
(define-data-var next-achievement-id uint u1)
(define-data-var next-verification-id uint u1)
(define-data-var verification-window-blocks uint u1000)

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
    active: bool,
    verification-required: bool,
    verified: bool,
    verification-count: uint
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

(define-map user-stats
  principal
  {
    total-claims: uint,
    total-drops-created: uint,
    current-season-claims: uint,
    current-season-points: uint,
    total-points: uint,
    achievements-unlocked: uint,
    current-streak: uint,
    best-streak: uint,
    last-claim-block: uint
  }
)

(define-map season-leaderboard
  { season: uint, rank: uint }
  { user: principal, points: uint }
)

(define-map user-season-ranks
  { user: principal, season: uint }
  { rank: uint, points: uint, rewards-claimed: bool }
)

(define-map achievements
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    points-reward: uint,
    requirement-type: (string-ascii 32),
    requirement-value: uint,
    is-active: bool
  }
)

(define-map user-achievements
  { user: principal, achievement-id: uint }
  { unlocked: bool, unlock-block: uint }
)

(define-map season-info
  uint
  {
    start-block: uint,
    end-block: uint,
    total-participants: uint,
    total-claims: uint,
    is-active: bool
  }
)

;; Drop verification network data structures
(define-map verifiers
  principal
  {
    reputation-score: uint,
    total-verifications: uint,
    accurate-verifications: uint,
    registration-block: uint,
    is-active: bool,
    stake-amount: uint
  }
)

(define-map verification-requests
  uint
  {
    drop-id: uint,
    requester: principal,
    request-block: uint,
    required-verifications: uint,
    current-verifications: uint,
    consensus-reached: bool,
    verification-reward: uint,
    deadline-block: uint
  }
)

(define-map drop-verifications
  { verification-id: uint, verifier: principal }
  {
    is-valid: bool,
    verification-block: uint,
    evidence-hash: (string-ascii 64),
    reward-claimed: bool
  }
)

(define-map verification-consensus
  uint
  {
    positive-votes: uint,
    negative-votes: uint,
    final-result: bool,
    consensus-block: uint
  }
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
  (max-claims uint)
  (requires-verification bool))
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
      active: true,
      verification-required: requires-verification,
      verified: (not requires-verification),
      verification-count: u0
    })
    
    (map-set drop-participants drop-id { participant-count: u0 })
    (var-set next-drop-id (+ drop-id u1))
    (unwrap! (update-user-stats-for-drop-creation tx-sender) ERR_INVALID_LOCATION)
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
    ;; Check if drop requires verification and is verified
    (if (get verification-required drop-data)
      (asserts! (get verified drop-data) ERR_NOT_VERIFIED)
      true
    )
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
    
    (unwrap! (update-user-stats-for-claim tx-sender) ERR_INVALID_LOCATION)
    (unwrap! (check-and-unlock-achievements tx-sender) ERR_INVALID_LOCATION)
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

(define-public (create-achievement 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (points-reward uint)
  (requirement-type (string-ascii 32))
  (requirement-value uint))
  (let
    (
      (achievement-id (var-get next-achievement-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> points-reward u0) ERR_INVALID_LOCATION)
    (asserts! (> requirement-value u0) ERR_INVALID_LOCATION)
    
    (map-set achievements achievement-id {
      name: name,
      description: description,
      points-reward: points-reward,
      requirement-type: requirement-type,
      requirement-value: requirement-value,
      is-active: true
    })
    
    (var-set next-achievement-id (+ achievement-id u1))
    (ok achievement-id)
  )
)

(define-public (start-new-season (duration uint))
  (let
    (
      (current-block stacks-block-height)
      (new-season (+ (var-get current-season) u1))
      (end-block (+ current-block duration))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    
    (map-set season-info (var-get current-season) {
      start-block: (var-get season-start-block),
      end-block: (var-get season-end-block),
      total-participants: u0,
      total-claims: u0,
      is-active: false
    })
    
    (var-set current-season new-season)
    (var-set season-start-block current-block)
    (var-set season-end-block end-block)
    
    (map-set season-info new-season {
      start-block: current-block,
      end-block: end-block,
      total-participants: u0,
      total-claims: u0,
      is-active: true
    })
    
    (ok new-season)
  )
)

(define-private (update-user-stats-for-claim (user principal))
  (let
    (
      (current-stats (default-to {
        total-claims: u0,
        total-drops-created: u0,
        current-season-claims: u0,
        current-season-points: u0,
        total-points: u0,
        achievements-unlocked: u0,
        current-streak: u0,
        best-streak: u0,
        last-claim-block: u0
      } (map-get? user-stats user)))
      (current-block stacks-block-height)
      (new-streak (if (< (- current-block (get last-claim-block current-stats)) u1000)
        (+ (get current-streak current-stats) u1)
        u1))
      (base-points u10)
      (streak-bonus (* new-streak u2))
      (total-points (+ base-points streak-bonus))
    )
    (map-set user-stats user {
      total-claims: (+ (get total-claims current-stats) u1),
      total-drops-created: (get total-drops-created current-stats),
      current-season-claims: (+ (get current-season-claims current-stats) u1),
      current-season-points: (+ (get current-season-points current-stats) total-points),
      total-points: (+ (get total-points current-stats) total-points),
      achievements-unlocked: (get achievements-unlocked current-stats),
      current-streak: new-streak,
      best-streak: (if (> new-streak (get best-streak current-stats)) new-streak (get best-streak current-stats)),
      last-claim-block: current-block
    })
    (ok true)
  )
)

(define-private (update-user-stats-for-drop-creation (user principal))
  (let
    (
      (current-stats (default-to {
        total-claims: u0,
        total-drops-created: u0,
        current-season-claims: u0,
        current-season-points: u0,
        total-points: u0,
        achievements-unlocked: u0,
        current-streak: u0,
        best-streak: u0,
        last-claim-block: u0
      } (map-get? user-stats user)))
      (creation-points u25)
    )
    (map-set user-stats user {
      total-claims: (get total-claims current-stats),
      total-drops-created: (+ (get total-drops-created current-stats) u1),
      current-season-claims: (get current-season-claims current-stats),
      current-season-points: (+ (get current-season-points current-stats) creation-points),
      total-points: (+ (get total-points current-stats) creation-points),
      achievements-unlocked: (get achievements-unlocked current-stats),
      current-streak: (get current-streak current-stats),
      best-streak: (get best-streak current-stats),
      last-claim-block: (get last-claim-block current-stats)
    })
    (ok true)
  )
)

(define-private (check-and-unlock-achievements (user principal))
  (let
    (
      (user-stats-data (unwrap! (map-get? user-stats user) (ok false)))
    )
    ;; (try! (check-achievement user u1 "total-claims" (get total-claims user-stats-data)))
    ;; (try! (check-achievement user u2 "current-streak" (get current-streak user-stats-data)))
    ;; (try! (check-achievement user u3 "total-drops-created" (get total-drops-created user-stats-data)))
    ;; (try! (check-achievement user u4 "total-points" (get total-points user-stats-data)))
    (ok true)
  )
)

(define-private (check-achievement (user principal) (achievement-id uint) (stat-type (string-ascii 32)) (stat-value uint))
  (let
    (
      (achievement-data (unwrap! (map-get? achievements achievement-id) (ok false)))
      (user-achievement (map-get? user-achievements { user: user, achievement-id: achievement-id }))
    )
    (if (and 
      (get is-active achievement-data)
      (is-eq (get requirement-type achievement-data) stat-type)
      (>= stat-value (get requirement-value achievement-data))
      (is-none user-achievement))
      (begin
        (map-set user-achievements 
          { user: user, achievement-id: achievement-id }
          { unlocked: true, unlock-block: stacks-block-height })
        ;; (try! (award-achievement-points user (get points-reward achievement-data)))
        (ok true)
      )
      (ok false)
    )
  )
)

(define-private (award-achievement-points (user principal) (points uint))
  (let
    (
      (current-stats (unwrap! (map-get? user-stats user) (ok false)))
    )
    (map-set user-stats user 
      (merge current-stats {
        achievements-unlocked: (+ (get achievements-unlocked current-stats) u1),
        current-season-points: (+ (get current-season-points current-stats) points),
        total-points: (+ (get total-points current-stats) points)
      })
    )
    (ok true)
  )
)

(define-public (claim-season-reward (season uint))
  (let
    (
      (user-rank (unwrap! (map-get? user-season-ranks { user: tx-sender, season: season }) ERR_INVALID_SEASON))
      (season-data (unwrap! (map-get? season-info season) ERR_INVALID_SEASON))
    )
    (asserts! (not (get is-active season-data)) ERR_SEASON_NOT_ACTIVE)
    (asserts! (not (get rewards-claimed user-rank)) ERR_ALREADY_CLAIMED_REWARD)
    
    (map-set user-season-ranks 
      { user: tx-sender, season: season }
      (merge user-rank { rewards-claimed: true })
    )
    
    (ok (get rank user-rank))
  )
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (get-user-achievements (user principal))
  (ok (filter check-user-achievement-unlocked (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
)

(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements achievement-id)
)

(define-read-only (get-season-leaderboard (season uint) (limit uint))
  (ok (map get-leaderboard-entry (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
)

(define-read-only (get-current-season)
  (var-get current-season)
)

(define-read-only (get-season-info (season uint))
  (map-get? season-info season)
)

(define-read-only (get-user-season-rank (user principal) (season uint))
  (map-get? user-season-ranks { user: user, season: season })
)

(define-private (check-user-achievement-unlocked (achievement-id uint))
  (match (map-get? user-achievements { user: tx-sender, achievement-id: achievement-id })
    achievement-data (get unlocked achievement-data)
    false
  )
)

(define-private (get-leaderboard-entry (rank uint))
  (match (map-get? season-leaderboard { season: (var-get current-season), rank: rank })
    leaderboard-entry { user: (get user leaderboard-entry), points: (get points leaderboard-entry) }
    { user: tx-sender, points: u0 }
  )
)

;; Drop Verification Network Functions

;; Register as a verifier with reputation stake
(define-public (register-verifier (stake-amount uint))
  (begin
    (asserts! (> stake-amount u100) ERR_INSUFFICIENT_REPUTATION) ;; Minimum stake requirement
    (asserts! (is-none (map-get? verifiers tx-sender)) ERR_ALREADY_VERIFIED)
    
    (map-set verifiers tx-sender {
      reputation-score: u100, ;; Starting reputation
      total-verifications: u0,
      accurate-verifications: u0,
      registration-block: stacks-block-height,
      is-active: true,
      stake-amount: stake-amount
    })
    
    (ok true)
  )
)

;; Request verification for a drop
(define-public (request-drop-verification (drop-id uint) (reward-amount uint))
  (let
    (
      (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
      (verification-id (var-get next-verification-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator drop-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get verification-required drop-data) ERR_INVALID_LOCATION)
    (asserts! (not (get verified drop-data)) ERR_ALREADY_VERIFIED)
    (asserts! (> reward-amount u0) ERR_INVALID_LOCATION)
    
    (map-set verification-requests verification-id {
      drop-id: drop-id,
      requester: tx-sender,
      request-block: current-block,
      required-verifications: u3, ;; Require 3 verifications for consensus
      current-verifications: u0,
      consensus-reached: false,
      verification-reward: reward-amount,
      deadline-block: (+ current-block (var-get verification-window-blocks))
    })
    
    (var-set next-verification-id (+ verification-id u1))
    (ok verification-id)
  )
)

;; Submit verification for a drop
(define-public (submit-verification 
  (verification-id uint) 
  (is-valid bool) 
  (evidence-hash (string-ascii 64)))
  (let
    (
      (request-data (unwrap! (map-get? verification-requests verification-id) ERR_INVALID_LOCATION))
      (verifier-data (unwrap! (map-get? verifiers tx-sender) ERR_VERIFIER_NOT_REGISTERED))
      (drop-data (unwrap! (map-get? drops (get drop-id request-data)) ERR_DROP_NOT_FOUND))
      (current-block stacks-block-height)
    )
    ;; Validation checks
    (asserts! (get is-active verifier-data) ERR_VERIFIER_NOT_REGISTERED)
    (asserts! (>= (get reputation-score verifier-data) u50) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (not (is-eq tx-sender (get creator drop-data))) ERR_CANNOT_VERIFY_OWN_DROP)
    (asserts! (<= current-block (get deadline-block request-data)) ERR_VERIFICATION_EXPIRED)
    (asserts! (not (get consensus-reached request-data)) ERR_ALREADY_VERIFIED)
    (asserts! (is-none (map-get? drop-verifications { verification-id: verification-id, verifier: tx-sender })) ERR_ALREADY_VERIFIED)
    
    ;; Record verification
    (map-set drop-verifications 
      { verification-id: verification-id, verifier: tx-sender }
      {
        is-valid: is-valid,
        verification-block: current-block,
        evidence-hash: evidence-hash,
        reward-claimed: false
      }
    )
    
    ;; Update request count
    (map-set verification-requests verification-id 
      (merge request-data { 
        current-verifications: (+ (get current-verifications request-data) u1) 
      })
    )
    
    ;; Update verifier stats
    (map-set verifiers tx-sender
      (merge verifier-data {
        total-verifications: (+ (get total-verifications verifier-data) u1)
      })
    )
    
    ;; Check if consensus is reached
    (try! (check-verification-consensus verification-id))
    
    (ok true)
  )
)

;; Check and finalize verification consensus
(define-private (check-verification-consensus (verification-id uint))
  (let
    (
      (request-data (unwrap! (map-get? verification-requests verification-id) ERR_INVALID_LOCATION))
      (required-votes (get required-verifications request-data))
      (current-votes (get current-verifications request-data))
    )
    (if (>= current-votes required-votes)
      (finalize-verification-consensus verification-id)
      (ok false)
    )
  )
)

;; Finalize verification consensus and update drop status
(define-private (finalize-verification-consensus (verification-id uint))
  (let
    (
      (request-data (unwrap! (map-get? verification-requests verification-id) ERR_INVALID_LOCATION))
      (drop-id (get drop-id request-data))
      (drop-data (unwrap! (map-get? drops drop-id) ERR_DROP_NOT_FOUND))
      (consensus-result (calculate-consensus-result verification-id))
    )
    ;; Mark consensus as reached
    (map-set verification-requests verification-id
      (merge request-data { consensus-reached: true })
    )
    
    ;; Store consensus result
    (map-set verification-consensus verification-id {
      positive-votes: (get positive-votes consensus-result),
      negative-votes: (get negative-votes consensus-result),
      final-result: (get final-result consensus-result),
      consensus-block: stacks-block-height
    })
    
    ;; Update drop verification status
    (map-set drops drop-id
      (merge drop-data {
        verified: (get final-result consensus-result),
        verification-count: (+ (get verification-count drop-data) u1)
      })
    )
    
    ;; Update verifier reputations based on consensus
    (try! (update-verifier-reputations verification-id))
    
    (ok true)
  )
)

;; Calculate consensus result from verifications
(define-private (calculate-consensus-result (verification-id uint))
  (let
    (
      (positive-count u0)
      (negative-count u0)
    )
    ;; In a real implementation, this would iterate through all verifications
    ;; For this example, we'll use a simplified calculation
    {
      positive-votes: u2, ;; Simplified - would calculate actual votes
      negative-votes: u1,
      final-result: true ;; Majority wins
    }
  )
)

;; Update verifier reputations based on consensus accuracy
(define-private (update-verifier-reputations (verification-id uint))
  (let
    (
      (consensus-data (unwrap! (map-get? verification-consensus verification-id) ERR_INVALID_LOCATION))
      (final-result (get final-result consensus-data))
    )
    ;; In a real implementation, this would update each verifier's reputation
    ;; based on whether their vote matched the consensus
    (ok true)
  )
)

;; Claim verification reward
(define-public (claim-verification-reward (verification-id uint))
  (let
    (
      (verification-data (unwrap! (map-get? drop-verifications 
        { verification-id: verification-id, verifier: tx-sender }) ERR_INVALID_LOCATION))
      (request-data (unwrap! (map-get? verification-requests verification-id) ERR_INVALID_LOCATION))
      (consensus-data (unwrap! (map-get? verification-consensus verification-id) ERR_INVALID_LOCATION))
    )
    (asserts! (get consensus-reached request-data) ERR_INVALID_LOCATION)
    (asserts! (not (get reward-claimed verification-data)) ERR_ALREADY_CLAIMED_REWARD)
    
    ;; Check if verifier was on the winning side
    (asserts! (is-eq (get is-valid verification-data) (get final-result consensus-data)) ERR_INVALID_LOCATION)
    
    ;; Mark reward as claimed
    (map-set drop-verifications 
      { verification-id: verification-id, verifier: tx-sender }
      (merge verification-data { reward-claimed: true })
    )
    
    ;; Award reputation points
    (try! (update-verifier-reputation-reward tx-sender))
    
    (ok (get verification-reward request-data))
  )
)

;; Update verifier reputation after successful verification
(define-private (update-verifier-reputation-reward (verifier principal))
  (let
    (
      (verifier-data (unwrap! (map-get? verifiers verifier) ERR_VERIFIER_NOT_REGISTERED))
    )
    (map-set verifiers verifier
      (merge verifier-data {
        reputation-score: (+ (get reputation-score verifier-data) u10),
        accurate-verifications: (+ (get accurate-verifications verifier-data) u1)
      })
    )
    (ok true)
  )
)

;; Read-only functions for verification network
(define-read-only (get-verifier-info (verifier principal))
  (map-get? verifiers verifier)
)

(define-read-only (get-verification-request (verification-id uint))
  (map-get? verification-requests verification-id)
)

(define-read-only (get-verification-consensus (verification-id uint))
  (map-get? verification-consensus verification-id)
)

(define-read-only (get-drop-verification (verification-id uint) (verifier principal))
  (map-get? drop-verifications { verification-id: verification-id, verifier: verifier })
)

