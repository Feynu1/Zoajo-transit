;; ZOAJO TRANSIT - DECENTRALIZED RIDE & BUS TICKETING SYSTEM
;; =================================================================
;; A comprehensive smart contract system for African informal transport
;; Supporting danfos, matatus, tro-tros and other local transport
;; =================================================================

;; =================================================================
;; CONSTANTS & ERROR CODES
;; =================================================================

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-PRICE (err u402))
(define-constant ERR-TICKET-NOT-FOUND (err u403))
(define-constant ERR-TICKET-EXPIRED (err u404))
(define-constant ERR-TICKET-ALREADY-USED (err u405))
(define-constant ERR-INSUFFICIENT-BALANCE (err u406))
(define-constant ERR-ROUTE-NOT-EXISTS (err u407))
(define-constant ERR-VEHICLE-NOT-REGISTERED (err u408))
(define-constant ERR-DRIVER-NOT-VERIFIED (err u409))
(define-constant ERR-TRIP-NOT-ACTIVE (err u410))
(define-constant ERR-REFUND-PERIOD-EXPIRED (err u411))
(define-constant ERR-ALREADY-REFUNDED (err u412))
(define-constant ERR-VEHICLE-ALREADY-REGISTERED (err u413))
(define-constant ERR-DRIVER-NOT-FOUND (err u414))

;; Contract constants
(define-constant TICKET-EXPIRY-BLOCKS u144) ;; ~24 hours (10 min blocks)
(define-constant REFUND-PERIOD-BLOCKS u72)   ;; ~12 hours
(define-constant PLATFORM-FEE-BASIS-POINTS u250) ;; 2.5%

;; =================================================================
;; DATA VARIABLES
;; =================================================================

;; Capture deployer as owner at deploy-time (constants cannot rely on tx-sender)
(define-data-var contract-owner principal tx-sender)
(define-data-var next-route-id uint u1)
(define-data-var next-trip-id uint u1)
(define-data-var next-ticket-id uint u1)
(define-data-var platform-fee-collector principal (var-get contract-owner))
(define-data-var emergency-stop bool false)

;; =================================================================
;; DATA STRUCTURES
;; =================================================================

;; Route information
(define-map routes
    { route-id: uint }
    {
        name: (string-ascii 64),
        origin: (string-ascii 32),
        destination: (string-ascii 32),
        distance-km: uint,
        base-fare: uint,
        is-active: bool,
        created-at: uint
    }
)

;; Vehicle registration
(define-map vehicles
    { vehicle-id: (string-ascii 20) }
    {
        owner: principal,
        vehicle-type: (string-ascii 16), ;; "danfo", "matatu", "tro-tro", etc.
        capacity: uint,
        license-plate: (string-ascii 12),
        is-verified: bool,
        is-active: bool,
        created-at: uint
    }
)

;; Driver verification
(define-map drivers
    { driver-address: principal }
    {
        name: (string-ascii 64),
        license-number: (string-ascii 20),
        phone-number: (string-ascii 15),
        is-verified: bool,
        verification-date: uint,
        rating: uint, ;; Out of 100
        total-trips: uint
    }
)

;; Active trips
(define-map trips
    { trip-id: uint }
    {
        route-id: uint,
        vehicle-id: (string-ascii 20),
        driver: principal,
        departure-time: uint,
        estimated-arrival: uint,
        fare: uint,
        available-seats: uint,
        total-seats: uint,
        is-active: bool,
        is-completed: bool,
        created-at: uint
    }
)

;; Ticket purchases
;; NOTE: Use a numeric verification code to avoid string/buffer conversions that fail lint/compile.
(define-map tickets
    { ticket-id: uint }
    {
        passenger: principal,
        trip-id: uint,
        seat-number: uint,
        fare-paid: uint,
        purchase-time: uint,
        is-used: bool,
        is-refunded: bool,
        refund-amount: uint,
        verification-code: uint ;; 8-digit numeric code: 0..99,999,999
    }
)

;; Passenger profiles
(define-map passengers
    { passenger-address: principal }
    {
        name: (string-ascii 64),
        phone-number: (string-ascii 15),
        total-trips: uint,
        reputation-score: uint, ;; Out of 100
        created-at: uint
    }
)

;; Platform statistics
(define-map platform-stats
    { key: (string-ascii 20) }
    { value: uint }
)

;; =================================================================
;; PRIVATE HELPER FUNCTIONS
;; =================================================================

;; Simple deterministic verification code (avoids non-existent conversions like int-to-ascii)
(define-private (generate-verification-code (ticket-id uint))
    (mod (+ ticket-id block-height) u100000000)
)

;; Calculate platform fee
(define-private (calculate-platform-fee (amount uint))
    (/ (* amount PLATFORM-FEE-BASIS-POINTS) u10000)
)

;; Check if caller is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

;; Increment platform stats
(define-private (increment-stat (stat-key (string-ascii 20)))
    (let ((current-value (default-to u0 (get value (map-get? platform-stats {key: stat-key})))) )
        (map-set platform-stats {key: stat-key} {value: (+ current-value u1)})
    )
)

;; =================================================================
;; ROUTE MANAGEMENT FUNCTIONS
;; =================================================================

;; Create a new route (admin only)
(define-public (create-route 
    (name (string-ascii 64))
    (origin (string-ascii 32))
    (destination (string-ascii 32))
    (distance-km uint)
    (base-fare uint))
    (let ((route-id (var-get next-route-id)))
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (asserts! (> base-fare u0) ERR-INVALID-PRICE)
        (map-set routes
            {route-id: route-id}
            {
                name: name,
                origin: origin,
                destination: destination,
                distance-km: distance-km,
                base-fare: base-fare,
                is-active: true,
                created-at: block-height
            }
        )
        (var-set next-route-id (+ route-id u1))
        (increment-stat "total-routes")
        (ok route-id)
    )
)

;; Update route status (admin only)
(define-public (toggle-route-status (route-id uint))
    (let ((route (unwrap! (map-get? routes {route-id: route-id}) ERR-ROUTE-NOT-EXISTS)))
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (map-set routes
            {route-id: route-id}
            (merge route {is-active: (not (get is-active route))})
        )
        (ok true)
    )
)

;; =================================================================
;; VEHICLE & DRIVER MANAGEMENT
;; =================================================================

;; Register a vehicle
(define-public (register-vehicle 
    (vehicle-id (string-ascii 20))
    (vehicle-type (string-ascii 16))
    (capacity uint)
    (license-plate (string-ascii 12)))
    (begin
        (asserts! (> capacity u0) ERR-INVALID-PRICE)
        ;; Ensure this vehicle-id is new
        (asserts! (is-none (map-get? vehicles {vehicle-id: vehicle-id})) ERR-VEHICLE-ALREADY-REGISTERED)
        (map-set vehicles
            {vehicle-id: vehicle-id}
            {
                owner: tx-sender,
                vehicle-type: vehicle-type,
                capacity: capacity,
                license-plate: license-plate,
                is-verified: false,
                is-active: true,
                created-at: block-height
            }
        )
        (increment-stat "total-vehicles")
        (ok true)
    )
)

;; Verify vehicle (admin only)
(define-public (verify-vehicle (vehicle-id (string-ascii 20)))
    (let ((vehicle (unwrap! (map-get? vehicles {vehicle-id: vehicle-id}) ERR-VEHICLE-NOT-REGISTERED)))
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (map-set vehicles
            {vehicle-id: vehicle-id}
            (merge vehicle {is-verified: true})
        )
        (ok true)
    )
)

;; Register as driver
(define-public (register-driver
    (name (string-ascii 64))
    (license-number (string-ascii 20))
    (phone-number (string-ascii 15)))
    (begin
        (map-set drivers
            {driver-address: tx-sender}
            {
                name: name,
                license-number: license-number,
                phone-number: phone-number,
                is-verified: false,
                verification-date: u0,
                rating: u80, ;; Default starting rating
                total-trips: u0
            }
        )
        (increment-stat "total-drivers")
        (ok true)
    )
)

;; Verify driver (admin only)
(define-public (verify-driver (driver-address principal))
    (let ((driver (unwrap! (map-get? drivers {driver-address: driver-address}) ERR-DRIVER-NOT-FOUND)))
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (map-set drivers
            {driver-address: driver-address}
            (merge driver {
                is-verified: true,
                verification-date: block-height
            })
        )
        (ok true)
    )
)

;; =================================================================
;; TRIP MANAGEMENT
;; =================================================================

;; Create a new trip (driver must be verified and own the vehicle)
(define-public (create-trip
    (route-id uint)
    (vehicle-id (string-ascii 20))
    (departure-time uint)
    (estimated-arrival uint))
    (let (
        (trip-id (var-get next-trip-id))
        (route (unwrap! (map-get? routes {route-id: route-id}) ERR-ROUTE-NOT-EXISTS))
        (vehicle (unwrap! (map-get? vehicles {vehicle-id: vehicle-id}) ERR-VEHICLE-NOT-REGISTERED))
        (driver (unwrap! (map-get? drivers {driver-address: tx-sender}) ERR-DRIVER-NOT-FOUND))
    )
        (asserts! (not (var-get emergency-stop)) ERR-UNAUTHORIZED)
        (asserts! (get is-active route) ERR-ROUTE-NOT-EXISTS)
        (asserts! (get is-verified vehicle) ERR-VEHICLE-NOT-REGISTERED)
        (asserts! (get is-verified driver) ERR-DRIVER-NOT-VERIFIED)
        (asserts! (is-eq (get owner vehicle) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (> departure-time block-height) ERR-INVALID-PRICE)
        (map-set trips
            {trip-id: trip-id}
            {
                route-id: route-id,
                vehicle-id: vehicle-id,
                driver: tx-sender,
                departure-time: departure-time,
                estimated-arrival: estimated-arrival,
                fare: (get base-fare route),
                available-seats: (get capacity vehicle),
                total-seats: (get capacity vehicle),
                is-active: true,
                is-completed: false,
                created-at: block-height
            }
        )
        (var-set next-trip-id (+ trip-id u1))
        (increment-stat "total-trips")
        (ok trip-id)
    )
)

;; Complete a trip
(define-public (complete-trip (trip-id uint))
    (let ((trip (unwrap! (map-get? trips {trip-id: trip-id}) ERR-TRIP-NOT-ACTIVE)))
        (asserts! (is-eq (get driver trip) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get is-active trip) ERR-TRIP-NOT-ACTIVE)
        (map-set trips
            {trip-id: trip-id}
            (merge trip {
                is-active: false,
                is-completed: true
            })
        )
        ;; Update driver stats
        (let ((driver (unwrap! (map-get? drivers {driver-address: tx-sender}) ERR-DRIVER-NOT-FOUND)))
            (map-set drivers
                {driver-address: tx-sender}
                (merge driver {total-trips: (+ (get total-trips driver) u1)})
            )
        )
        (increment-stat "completed-trips")
        (ok true)
    )
)

;; =================================================================
;; TICKET PURCHASING & MANAGEMENT
;; =================================================================

;; Purchase a ticket (escrow funds in contract)
(define-public (purchase-ticket (trip-id uint) (seat-number uint))
    (let (
        (ticket-id (var-get next-ticket-id))
        (trip (unwrap! (map-get? trips {trip-id: trip-id}) ERR-TRIP-NOT-ACTIVE))
        (fare (get fare trip))
        (verification-code (generate-verification-code ticket-id))
    )
        (asserts! (not (var-get emergency-stop)) ERR-UNAUTHORIZED)
        (asserts! (get is-active trip) ERR-TRIP-NOT-ACTIVE)
        (asserts! (> (get available-seats trip) u0) ERR-TICKET-NOT-FOUND)
        (asserts! (and (>= seat-number u1) (<= seat-number (get total-seats trip))) ERR-INVALID-PRICE)
        (asserts! (> (get departure-time trip) block-height) ERR-TICKET-EXPIRED)
        ;; escrow payment to contract
        (try! (stx-transfer? fare tx-sender (as-contract tx-sender)))
        ;; Create ticket
        (map-set tickets
            {ticket-id: ticket-id}
            {
                passenger: tx-sender,
                trip-id: trip-id,
                seat-number: seat-number,
                fare-paid: fare,
                purchase-time: block-height,
                is-used: false,
                is-refunded: false,
                refund-amount: u0,
                verification-code: verification-code
            }
        )
        ;; Update trip availability
        (map-set trips
            {trip-id: trip-id}
            (merge trip {available-seats: (- (get available-seats trip) u1)})
        )
        ;; Update/create passenger profile
        (match (map-get? passengers {passenger-address: tx-sender})
            existing-passenger (map-set passengers
                {passenger-address: tx-sender}
                (merge existing-passenger {total-trips: (+ (get total-trips existing-passenger) u1)})
            )
            (map-set passengers
                {passenger-address: tx-sender}
                {
                    name: "",
                    phone-number: "",
                    total-trips: u1,
                    reputation-score: u80,
                    created-at: block-height
                }
            )
        )
        (var-set next-ticket-id (+ ticket-id u1))
        (increment-stat "tickets-sold")
        (ok {ticket-id: ticket-id, verification-code: verification-code})
    )
)

;; Use/validate a ticket (driver validates; pays out escrow minus platform fee)
(define-public (use-ticket (ticket-id uint) (verification-code uint))
    (let (
        (ticket (unwrap! (map-get? tickets {ticket-id: ticket-id}) ERR-TICKET-NOT-FOUND))
        (trip (unwrap! (map-get? trips {trip-id: (get trip-id ticket)}) ERR-TRIP-NOT-ACTIVE))
    )
        (asserts! (is-eq (get driver trip) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get verification-code ticket) verification-code) ERR-UNAUTHORIZED)
        (asserts! (not (get is-used ticket)) ERR-TICKET-ALREADY-USED)
        (asserts! (not (get is-refunded ticket)) ERR-ALREADY-REFUNDED)
        ;; Mark ticket as used
        (map-set tickets
            {ticket-id: ticket-id}
            (merge ticket {is-used: true})
        )
        ;; Release payment to driver and platform
        (let (
            (fare (get fare-paid ticket))
            (platform-fee (calculate-platform-fee fare))
            (driver-payment (- fare platform-fee))
        )
            (try! (as-contract (stx-transfer? driver-payment tx-sender (get driver trip))))
            (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get platform-fee-collector))))
        )
        (increment-stat "tickets-used")
        (ok true)
    )
)

;; Request ticket refund (only before departure and within refund window)
(define-public (request-refund (ticket-id uint))
    (let (
        (ticket (unwrap! (map-get? tickets {ticket-id: ticket-id}) ERR-TICKET-NOT-FOUND))
        (trip (unwrap! (map-get? trips {trip-id: (get trip-id ticket)}) ERR-TRIP-NOT-ACTIVE))
    )
        (asserts! (is-eq (get passenger ticket) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (not (get is-used ticket)) ERR-TICKET-ALREADY-USED)
        (asserts! (not (get is-refunded ticket)) ERR-ALREADY-REFUNDED)
        ;; Must be within refund window: now <= purchase-time + REFUND-PERIOD-BLOCKS
        (asserts! (<= block-height (+ (get purchase-time ticket) REFUND-PERIOD-BLOCKS)) ERR-REFUND-PERIOD-EXPIRED)
        ;; Must be before departure
        (asserts! (> (get departure-time trip) block-height) ERR-TICKET-EXPIRED)
        (let (
            (refund-amount (- (get fare-paid ticket) (calculate-platform-fee (get fare-paid ticket))))
        )
            ;; Process refund from contract escrow to passenger
            (try! (as-contract (stx-transfer? refund-amount tx-sender (get passenger ticket))))
            ;; Update ticket
            (map-set tickets
                {ticket-id: ticket-id}
                (merge ticket {
                    is-refunded: true,
                    refund-amount: refund-amount
                })
            )
            ;; Update trip availability
            (map-set trips
                {trip-id: (get trip-id ticket)}
                (merge trip {available-seats: (+ (get available-seats trip) u1)})
            )
            (increment-stat "tickets-refunded")
            (ok refund-amount)
        )
    )
)

;; =================================================================
;; READ-ONLY FUNCTIONS
;; =================================================================

(define-read-only (get-route (route-id uint))
    (map-get? routes {route-id: route-id})
)

(define-read-only (get-vehicle (vehicle-id (string-ascii 20)))
    (map-get? vehicles {vehicle-id: vehicle-id})
)

(define-read-only (get-driver (driver-address principal))
    (map-get? drivers {driver-address: driver-address})
)

(define-read-only (get-trip (trip-id uint))
    (map-get? trips {trip-id: trip-id})
)

(define-read-only (get-ticket (ticket-id uint))
    (map-get? tickets {ticket-id: ticket-id})
)

(define-read-only (get-passenger (passenger-address principal))
    (map-get? passengers {passenger-address: passenger-address})
)

(define-read-only (get-platform-stat (stat-key (string-ascii 20)))
    (default-to u0 (get value (map-get? platform-stats {key: stat-key})))
)

(define-read-only (get-available-trips-for-route (route-id uint))
    (ok "Use external indexing for complex queries")
)

;; =================================================================
;; ADMIN FUNCTIONS
;; =================================================================

(define-public (toggle-emergency-stop)
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set emergency-stop (not (var-get emergency-stop)))
        (ok (var-get emergency-stop))
    )
)

(define-public (update-fee-collector (new-collector principal))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set platform-fee-collector new-collector)
        (ok true)
    )
)

(define-public (withdraw-platform-fees (amount uint))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (try! (as-contract (stx-transfer? amount tx-sender (var-get platform-fee-collector))))
        (ok true)
    )
)

;; =================================================================
;; INITIALIZATION
;; =================================================================

(begin
    (map-set platform-stats {key: "total-routes"} {value: u0})
    (map-set platform-stats {key: "total-vehicles"} {value: u0})
    (map-set platform-stats {key: "total-drivers"} {value: u0})
    (map-set platform-stats {key: "total-trips"} {value: u0})
    (map-set platform-stats {key: "tickets-sold"} {value: u0})
    (map-set platform-stats {key: "tickets-used"} {value: u0})
    (map-set platform-stats {key: "tickets-refunded"} {value: u0})
    (map-set platform-stats {key: "completed-trips"} {value: u0})
)
