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
