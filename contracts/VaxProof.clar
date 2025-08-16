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
(define-constant err-batch-recalled (err u110))
(define-constant err-batch-not-found (err u111))
(define-constant err-batch-already-recalled (err u112))

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
(define-data-var last-batch-recall-id uint u0)

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

(define-map batch-recalls
    uint
    {
        batch-number: (string-ascii 32),
        vaccine-type: (string-ascii 64),
        issuer: principal,
        recall-reason: (string-ascii 256),
        recalled-by: principal,
        recall-date: uint,
        affected-tokens: (list 100 uint),
        status: (string-ascii 32)
    }
)

(define-map recalled-batches (string-ascii 32) bool)

(define-map batch-to-recall-id (string-ascii 32) uint)

(define-map vaccination-batch-status
    uint
    {
        batch-number: (string-ascii 32),
        is-recalled: bool,
        recall-id: (optional uint)
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

(define-read-only (is-batch-recalled (batch-number (string-ascii 32)))
    (default-to false (map-get? recalled-batches batch-number))
)

(define-read-only (get-batch-recall (recall-id uint))
    (map-get? batch-recalls recall-id)
)

(define-read-only (get-batch-recall-by-batch (batch-number (string-ascii 32)))
    (match (map-get? batch-to-recall-id batch-number)
        recall-id (map-get? batch-recalls recall-id)
        none
    )
)

(define-read-only (get-vaccination-batch-status (token-id uint))
    (map-get? vaccination-batch-status token-id)
)

(define-read-only (get-last-batch-recall-id)
    (ok (var-get last-batch-recall-id))
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
        (asserts! (not (is-batch-recalled batch-number)) err-batch-recalled)
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
        (map-set vaccination-batch-status new-token-id {
            batch-number: batch-number,
            is-recalled: false,
            recall-id: none
        })
        (var-set last-token-id new-token-id)
        (ok new-token-id)
    )
)

(define-public (verify-vax-proof (token-id uint) (proof (buff 32)))
    (let (
        (record (unwrap! (map-get? vaccination-records token-id) err-not-found))
        (batch-status (unwrap! (map-get? vaccination-batch-status token-id) err-not-found))
    )
        (asserts! (<= stacks-block-height (get expiration record)) err-expired)
        (asserts! (is-eq proof (get verification-hash record)) err-unauthorized)
        (asserts! (not (get is-recalled batch-status)) err-batch-recalled)
        (asserts! (not (is-batch-recalled (get batch-number record))) err-batch-recalled)
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

(define-public (recall-vaccination-batch 
    (batch-number (string-ascii 32))
    (vaccine-type (string-ascii 64))
    (issuer principal)
    (recall-reason (string-ascii 256))
    (affected-tokens (list 100 uint)))
    (let
        (
            (new-recall-id (+ (var-get last-batch-recall-id) u1))
            (current-time stacks-block-height)
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> (len batch-number) u0) err-insufficient-requirements)
        (asserts! (> (len vaccine-type) u0) err-insufficient-requirements)
        (asserts! (> (len recall-reason) u0) err-insufficient-requirements)
        (asserts! (not (is-batch-recalled batch-number)) err-batch-already-recalled)
        (map-set batch-recalls new-recall-id {
            batch-number: batch-number,
            vaccine-type: vaccine-type,
            issuer: issuer,
            recall-reason: recall-reason,
            recalled-by: tx-sender,
            recall-date: current-time,
            affected-tokens: affected-tokens,
            status: "active"
        })
        (map-set recalled-batches batch-number true)
        (map-set batch-to-recall-id batch-number new-recall-id)
        (var-set last-batch-recall-id new-recall-id)
        (unwrap! (update-affected-vaccinations-status affected-tokens new-recall-id batch-number) err-insufficient-requirements)
        (ok new-recall-id)
    )
)

(define-public (update-batch-recall-status (recall-id uint) (new-status (string-ascii 32)))
    (let
        (
            (recall-record (unwrap! (map-get? batch-recalls recall-id) err-batch-not-found))
            (current-time stacks-block-height)
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> (len new-status) u0) err-insufficient-requirements)
        (map-set batch-recalls recall-id (merge recall-record {
            status: new-status
        }))
        (ok true)
    )
)

(define-public (reinstate-vaccination-batch (batch-number (string-ascii 32)))
    (let
        (
            (recall-id (unwrap! (map-get? batch-to-recall-id batch-number) err-batch-not-found))
            (recall-record (unwrap! (map-get? batch-recalls recall-id) err-batch-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-batch-recalled batch-number) err-batch-not-found)
        (map-delete recalled-batches batch-number)
        (map-set batch-recalls recall-id (merge recall-record {
            status: "reinstated"
        }))
        (unwrap! (reinstate-affected-vaccinations (get affected-tokens recall-record)) err-insufficient-requirements)
        (ok true)
    )
)

(define-private (update-affected-vaccinations-status (token-ids (list 100 uint)) (recall-id uint) (batch-number (string-ascii 32)))
    (begin
        (map update-single-vaccination-status token-ids)
        (ok true)
    )
)

(define-private (update-single-vaccination-status (token-id uint))
    (let
        (
            (batch-status (map-get? vaccination-batch-status token-id))
        )
        (match batch-status
            status (begin
                (map-set vaccination-batch-status token-id (merge status {
                    is-recalled: true,
                    recall-id: (map-get? batch-to-recall-id (get batch-number status))
                }))
                true
            )
            true
        )
    )
)

(define-private (reinstate-affected-vaccinations (token-ids (list 100 uint)))
    (begin
        (map reinstate-single-vaccination token-ids)
        (ok true)
    )
)

(define-private (reinstate-single-vaccination (token-id uint))
    (let
        (
            (batch-status (map-get? vaccination-batch-status token-id))
        )
        (match batch-status
            status (begin
                (map-set vaccination-batch-status token-id (merge status {
                    is-recalled: false,
                    recall-id: none
                }))
                true
            )
            true
        )
    )
)

(define-public (check-vaccination-recall-status (token-id uint))
    (let
        (
            (record (unwrap! (map-get? vaccination-records token-id) err-not-found))
            (batch-status (unwrap! (map-get? vaccination-batch-status token-id) err-not-found))
        )
        (ok {
            token-id: token-id,
            batch-number: (get batch-number record),
            is-batch-recalled: (is-batch-recalled (get batch-number record)),
            is-token-recalled: (get is-recalled batch-status),
            recall-id: (get recall-id batch-status)
        })
    )
)

(define-public (get-batch-recall-summary (batch-number (string-ascii 32)))
    (let
        (
            (is-recalled (is-batch-recalled batch-number))
            (recall-info (get-batch-recall-by-batch batch-number))
        )
        (ok {
            batch-number: batch-number,
            is-recalled: is-recalled,
            recall-details: recall-info
        })
    )
)

(define-public (bulk-verify-vaccinations (token-ids (list 50 uint)))
    (ok (map verify-single-vaccination-status token-ids))
)

(define-private (verify-single-vaccination-status (token-id uint))
    (let
        (
            (record (map-get? vaccination-records token-id))
            (batch-status (map-get? vaccination-batch-status token-id))
        )
        (match record
            vax-record 
                (match batch-status
                    b-status {
                        token-id: token-id,
                        is-valid: (and 
                            (<= stacks-block-height (get expiration vax-record))
                            (not (get is-recalled b-status))
                            (not (is-batch-recalled (get batch-number vax-record)))
                        ),
                        expiration: (get expiration vax-record),
                        is-recalled: (get is-recalled b-status),
                        batch-number: (get batch-number vax-record)
                    }
                    {
                        token-id: token-id,
                        is-valid: false,
                        expiration: u0,
                        is-recalled: false,
                        batch-number: ""
                    }
                )
            {
                token-id: token-id,
                is-valid: false,
                expiration: u0,
                is-recalled: false,
                batch-number: ""
            }
        )
    )
)


