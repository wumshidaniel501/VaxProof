(define-non-fungible-token vax-proof uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-expired (err u104))
(define-constant err-insufficient-requirements (err u105))
(define-constant err-not-organization-admin (err u106))
(define-constant err-organization-not-found (err u107))
(define-constant err-employee-not-found (err u108))
(define-constant err-requirement-not-found (err u109))

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
(define-data-var last-organization-id uint u0)
(define-data-var last-requirement-id uint u0)

(define-map organizations
    uint
    {
        name: (string-ascii 128),
        admin: principal,
        industry-type: (string-ascii 64),
        created-at: uint,
        active: bool
    }
)

(define-map organization-admins principal uint)

(define-map vaccination-requirements
    uint
    {
        organization-id: uint,
        vaccine-types: (list 10 (string-ascii 64)),
        minimum-doses: uint,
        validity-months: uint,
        mandatory: bool,
        created-at: uint
    }
)

(define-map employee-registrations
    {organization-id: uint, employee: principal}
    {
        registered-at: uint,
        compliance-status: (string-ascii 32),
        last-check: uint
    }
)

(define-map compliance-records
    {organization-id: uint, employee: principal, requirement-id: uint}
    {
        vaccination-tokens: (list 10 uint),
        compliance-date: uint,
        status: (string-ascii 32),
        notes: (string-ascii 256)
    }
)

(define-map organization-statistics
    uint
    {
        total-employees: uint,
        compliant-employees: uint,
        total-requirements: uint,
        last-updated: uint
    }
)

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

(define-read-only (get-organization (organization-id uint))
    (map-get? organizations organization-id)
)

(define-read-only (get-organization-by-admin (admin principal))
    (match (map-get? organization-admins admin)
        org-id (map-get? organizations org-id)
        none
    )
)

(define-read-only (get-vaccination-requirement (requirement-id uint))
    (map-get? vaccination-requirements requirement-id)
)

(define-read-only (get-employee-compliance-status (organization-id uint) (employee principal))
    (map-get? employee-registrations {organization-id: organization-id, employee: employee})
)

(define-read-only (get-compliance-record (organization-id uint) (employee principal) (requirement-id uint))
    (map-get? compliance-records {organization-id: organization-id, employee: employee, requirement-id: requirement-id})
)

(define-read-only (get-organization-statistics (organization-id uint))
    (map-get? organization-statistics organization-id)
)

(define-public (register-issuer (issuer principal) (name (string-ascii 64)) (license-number (string-ascii 32)) (country (string-ascii 2)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> (len name) u0) err-insufficient-requirements)
        (asserts! (> (len license-number) u0) err-insufficient-requirements)
        (asserts! (is-eq (len country) u2) err-insufficient-requirements)
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
        (asserts! (> (len vaccine-type) u0) err-insufficient-requirements)
        (asserts! (> (len batch-number) u0) err-insufficient-requirements)
        (asserts! (> validity-period u0) err-insufficient-requirements)
        (asserts! (is-eq (len verification-hash) u32) err-insufficient-requirements)
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

(define-public (register-organization (name (string-ascii 128)) (industry-type (string-ascii 64)))
    (let
        (
            (new-org-id (+ (var-get last-organization-id) u1))
            (current-time stacks-block-height)
        )
        (asserts! (> (len name) u0) err-insufficient-requirements)
        (asserts! (> (len industry-type) u0) err-insufficient-requirements)
        (map-set organizations new-org-id {
            name: name,
            admin: tx-sender,
            industry-type: industry-type,
            created-at: current-time,
            active: true
        })
        (map-set organization-admins tx-sender new-org-id)
        (map-set organization-statistics new-org-id {
            total-employees: u0,
            compliant-employees: u0,
            total-requirements: u0,
            last-updated: current-time
        })
        (var-set last-organization-id new-org-id)
        (ok new-org-id)
    )
)

(define-public (create-vaccination-requirement 
    (organization-id uint)
    (vaccine-types (list 10 (string-ascii 64)))
    (minimum-doses uint)
    (validity-months uint)
    (mandatory bool))
    (let
        (
            (new-requirement-id (+ (var-get last-requirement-id) u1))
            (current-time stacks-block-height)
            (org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
        )
        (asserts! (is-eq tx-sender (get admin org)) err-not-organization-admin)
        (asserts! (> (len vaccine-types) u0) err-insufficient-requirements)
        (asserts! (> minimum-doses u0) err-insufficient-requirements)
        (asserts! (> validity-months u0) err-insufficient-requirements)
        (map-set vaccination-requirements new-requirement-id {
            organization-id: organization-id,
            vaccine-types: vaccine-types,
            minimum-doses: minimum-doses,
            validity-months: validity-months,
            mandatory: mandatory,
            created-at: current-time
        })
        (let
            (
                (current-stats (unwrap! (map-get? organization-statistics organization-id) err-organization-not-found))
                (updated-req-count (+ (get total-requirements current-stats) u1))
            )
            (map-set organization-statistics organization-id (merge current-stats {
                total-requirements: updated-req-count,
                last-updated: current-time
            }))
        )
        (var-set last-requirement-id new-requirement-id)
        (ok new-requirement-id)
    )
)

(define-public (register-employee (organization-id uint) (employee principal))
    (let
        (
            (current-time stacks-block-height)
            (org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
        )
        (asserts! (is-eq tx-sender (get admin org)) err-not-organization-admin)
        (map-set employee-registrations {organization-id: organization-id, employee: employee} {
            registered-at: current-time,
            compliance-status: "pending",
            last-check: current-time
        })
        (let
            (
                (current-stats (unwrap! (map-get? organization-statistics organization-id) err-organization-not-found))
                (updated-emp-count (+ (get total-employees current-stats) u1))
            )
            (map-set organization-statistics organization-id (merge current-stats {
                total-employees: updated-emp-count,
                last-updated: current-time
            }))
        )
        (ok true)
    )
)

(define-public (verify-employee-compliance 
    (organization-id uint)
    (employee principal)
    (requirement-id uint)
    (vaccination-tokens (list 10 uint)))
    (let
        (
            (current-time stacks-block-height)
            (org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
            (requirement (unwrap! (map-get? vaccination-requirements requirement-id) err-requirement-not-found))
            (employee-reg (unwrap! (map-get? employee-registrations {organization-id: organization-id, employee: employee}) err-employee-not-found))
        )
        (asserts! (is-eq tx-sender (get admin org)) err-not-organization-admin)
        (asserts! (is-eq (get organization-id requirement) organization-id) err-requirement-not-found)
        (asserts! (>= (len vaccination-tokens) (get minimum-doses requirement)) err-insufficient-requirements)
        (map-set compliance-records 
            {organization-id: organization-id, employee: employee, requirement-id: requirement-id} 
            {
                vaccination-tokens: vaccination-tokens,
                compliance-date: current-time,
                status: "compliant",
                notes: "Vaccination requirements verified"
            }
        )
        (map-set employee-registrations {organization-id: organization-id, employee: employee} (merge employee-reg {
            compliance-status: "compliant",
            last-check: current-time
        }))
        (ok true)
    )
)

(define-public (update-compliance-status 
    (organization-id uint)
    (employee principal)
    (status (string-ascii 32))
    (notes (string-ascii 256)))
    (let
        (
            (current-time stacks-block-height)
            (org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
            (employee-reg (unwrap! (map-get? employee-registrations {organization-id: organization-id, employee: employee}) err-employee-not-found))
        )
        (asserts! (is-eq tx-sender (get admin org)) err-not-organization-admin)
        (asserts! (> (len status) u0) err-insufficient-requirements)
        (map-set employee-registrations {organization-id: organization-id, employee: employee} (merge employee-reg {
            compliance-status: status,
            last-check: current-time
        }))
        (ok true)
    )
)

(define-public (generate-compliance-report (organization-id uint))
    (let
        (
            (current-time stacks-block-height)
            (org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
            (current-stats (unwrap! (map-get? organization-statistics organization-id) err-organization-not-found))
        )
        (asserts! (is-eq tx-sender (get admin org)) err-not-organization-admin)
        (map-set organization-statistics organization-id (merge current-stats {
            last-updated: current-time
        }))
        (ok current-stats)
    )
)