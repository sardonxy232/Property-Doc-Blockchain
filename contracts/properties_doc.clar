;; Properties Documentation Smart Contract
;; Blockchain solution for immutable property documentation management
;; Created: April 2025
;; Version: 1.0


;; ================================================
;; DATA STRUCTURES
;; ================================================

;; Registry Counter
(define-data-var document-counter uint u0)

;; Main Document Registry
(define-map real-estate-documents
  { doc-id: uint }
  {
    title: (string-ascii 64),
    owner: principal,
    file-size: uint,
    registration-block: uint,
    description: (string-ascii 128),
    tags: (list 10 (string-ascii 32))
  }
)

;; Document Permissions Registry
(define-map document-permissions
  { doc-id: uint, viewer: principal }
  { access-allowed: bool }
)

;; ================================================
;; CONSTANT DEFINITIONS AND ERROR CODES
;; ================================================

;; System Administrator
(define-constant admin-address tx-sender)

;; Error Response Identifiers
(define-constant error-doc-not-found (err u301))
(define-constant error-doc-already-exists (err u302))
(define-constant error-invalid-title (err u303))
(define-constant error-invalid-filesize (err u304))
(define-constant error-unauthorized-access (err u305))
(define-constant error-not-document-owner (err u306))
(define-constant error-admin-only-function (err u300))
(define-constant error-view-restricted (err u307))
(define-constant error-metadata-format (err u308))

;; ================================================
;; PRIVATE HELPER FUNCTIONS
;; ================================================

;; Checks if a tag is properly formatted
(define-private (valid-tag-format (tag (string-ascii 32)))
  (and
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Validates all tags in a collection
(define-private (validate-tags-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter valid-tag-format tags)) (len tags))
  )
)

;; Checks if document exists in registry
(define-private (document-registered (doc-id uint))
  (is-some (map-get? real-estate-documents { doc-id: doc-id }))
)

;; Verifies ownership of document
(define-private (is-document-owner (doc-id uint) (user principal))
  (match (map-get? real-estate-documents { doc-id: doc-id })
    doc-data (is-eq (get owner doc-data) user)
    false
  )
)

;; Gets file size for a document
(define-private (get-document-size (doc-id uint))
  (default-to u0
    (get file-size
      (map-get? real-estate-documents { doc-id: doc-id })
    )
  )
)
