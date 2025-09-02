;; =================================================================
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

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant TICKET-EXPIRY-BLOCKS u144) ;; ~24 hours (10 min blocks)
(define-constant REFUND-PERIOD-BLOCKS u72)   ;; ~12 hours
(define-constant PLATFORM-FEE-BASIS-POINTS u250) ;; 2.5%


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
        verification-code: (string-ascii 8)
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

;; DATA VARIABLES
;; =================================================================

(define-data-var next-route-id uint u1)
(define-data-var next-trip-id uint u1)
(define-data-var next-ticket-id uint u1)
(define-data-var platform-fee-collector principal CONTRACT-OWNER)
(define-data-var emergency-stop bool false)

;; =================================================================
;; PRIVATE HELPER FUNCTIONS
;; =================================================================

;; Calculate platform fee
(define-private (calculate-platform-fee (amount uint))
    (/ (* amount PLATFORM-FEE-BASIS-POINTS) u10000)
)

;; Check if caller is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

;; Increment platform stats
(define-private (increment-stat (stat-key (string-ascii 20)))
    (let ((current-value (default-to u0 (get value (map-get? platform-stats {key: stat-key})))))
        (map-set platform-stats {key: stat-key} {value: (+ current-value u1)})
    )
)

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

;; Update route status
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
        (asserts! (is-none (map-get? vehicles {vehicle-id: vehicle-id})) ERR-VEHICLE-NOT-REGISTERED)
        
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
    (let ((driver (unwrap! (map-get? drivers {driver-address: driver-address}) ERR-DRIVER-NOT-VERIFIED)))
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

