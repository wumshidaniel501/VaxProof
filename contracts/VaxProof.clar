(define-non-fungible-token vax-proof uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-expired (err u104))

(define-map vaccination-records
    uint
    {
        patient: principal,
        issuer: principal,
        vaccine-type: (string-ascii 64),
        batch-number: (string-ascii 32),
        date-issued: uint,
        expiration: uint,
        verification-hash: (buff 32)
    }
)

(define-map authorized-issuers principal bool)

(define-map issuer-details
    principal
    {
        name: (string-ascii 64),
        license-number: (string-ascii 32),
        country: (string-ascii 2)
    }
)

(define-data-var last-token-id uint u0)

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-vaccination-record (token-id uint))
    (match (map-get? vaccination-records token-id)
        record (ok record)
        (err err-not-found)
    )
)

(define-read-only (is-authorized-issuer (issuer principal))
    (default-to false (map-get? authorized-issuers issuer))
)

(define-read-only (get-issuer-details (issuer principal))
    (map-get? issuer-details issuer)
)

(define-public (register-issuer (issuer principal) (name (string-ascii 64)) (license-number (string-ascii 32)) (country (string-ascii 2)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-issuers issuer true)
        (map-set issuer-details issuer {
            name: name,
            license-number: license-number,
            country: country
        })
        (ok true)
    )
)

(define-public (remove-issuer (issuer principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete authorized-issuers issuer)
        (map-delete issuer-details issuer)
        (ok true)
    )
)

(define-public (mint-vax-proof
        (patient principal)
        (vaccine-type (string-ascii 64))
        (batch-number (string-ascii 32))
        (validity-period uint)
        (verification-hash (buff 32)))
    (let
        (
            (new-token-id (+ (var-get last-token-id) u1))
            (current-time stacks-block-height)
            (expiration-time (+ current-time validity-period))
        )
        (asserts! (is-authorized-issuer tx-sender) err-unauthorized)
        (try! (nft-mint? vax-proof new-token-id patient))
        (map-set vaccination-records new-token-id {
            patient: patient,
            issuer: tx-sender,
            vaccine-type: vaccine-type,
            batch-number: batch-number,
            date-issued: current-time,
            expiration: expiration-time,
            verification-hash: verification-hash
        })
        (var-set last-token-id new-token-id)
        (ok new-token-id)
    )
)

(define-public (verify-vax-proof (token-id uint) (proof (buff 32)))
    (let (
        (record (unwrap! (map-get? vaccination-records token-id) err-not-found))
    )
        (asserts! (<= stacks-block-height (get expiration record)) err-expired)
        (asserts! (is-eq proof (get verification-hash record)) err-unauthorized)
        (ok true)
    )
)

(define-public (revoke-vax-proof (token-id uint))
    (let (
        (record (unwrap! (map-get? vaccination-records token-id) err-not-found))
    )
        (asserts! (or 
            (is-eq tx-sender (get issuer record))
            (is-eq tx-sender contract-owner)
        ) err-unauthorized)
        (try! (nft-burn? vax-proof token-id (get patient record)))
        (map-delete vaccination-records token-id)
        (ok true)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-unauthorized)
        (try! (nft-transfer? vax-proof token-id sender recipient))
        (ok true)
    )
)