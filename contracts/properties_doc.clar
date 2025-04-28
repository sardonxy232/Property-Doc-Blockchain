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

;; ================================================
;; PUBLIC DOCUMENT MANAGEMENT FUNCTIONS
;; ================================================

;; Register a new real estate document
(define-public (register-document
  (title (string-ascii 64))
  (file-size uint)
  (description (string-ascii 128))
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (next-id (+ (var-get document-counter) u1))
    )
    ;; Validate inputs
    (asserts! (> (len title) u0) error-invalid-title)
    (asserts! (< (len title) u65) error-invalid-title)
    (asserts! (> file-size u0) error-invalid-filesize)
    (asserts! (< file-size u1000000000) error-invalid-filesize)
    (asserts! (> (len description) u0) error-invalid-title)
    (asserts! (< (len description) u129) error-invalid-title)
    (asserts! (validate-tags-collection tags) error-metadata-format)

    ;; Create document entry
    (map-insert real-estate-documents
      { doc-id: next-id }
      {
        title: title,
        owner: tx-sender,
        file-size: file-size,
        registration-block: block-height,
        description: description,
        tags: tags
      }
    )

    ;; Grant access to document creator
    (map-insert document-permissions
      { doc-id: next-id, viewer: tx-sender }
      { access-allowed: true }
    )

    ;; Update document counter
    (var-set document-counter next-id)
    (ok next-id)
  )
)

;; Update an existing document's information
(define-public (update-document
  (doc-id uint)
  (new-title (string-ascii 64))
  (new-file-size uint)
  (new-description (string-ascii 128))
  (new-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (doc-data (unwrap! (map-get? real-estate-documents { doc-id: doc-id })
        error-doc-not-found))
    )
    ;; Validate ownership and inputs
    (asserts! (document-registered doc-id) error-doc-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) error-not-document-owner)
    (asserts! (> (len new-title) u0) error-invalid-title)
    (asserts! (< (len new-title) u65) error-invalid-title)
    (asserts! (> new-file-size u0) error-invalid-filesize)
    (asserts! (< new-file-size u1000000000) error-invalid-filesize)
    (asserts! (> (len new-description) u0) error-invalid-title)
    (asserts! (< (len new-description) u129) error-invalid-title)
    (asserts! (validate-tags-collection new-tags) error-metadata-format)

    ;; Update document with new information
    (map-set real-estate-documents
      { doc-id: doc-id }
      (merge doc-data {
        title: new-title,
        file-size: new-file-size,
        description: new-description,
        tags: new-tags
      })
    )
    (ok true)
  )
)

;; Transfer document ownership to another user
(define-public (transfer-document-ownership (doc-id uint) (new-owner principal))
  (let
    (
      (doc-data (unwrap! (map-get? real-estate-documents { doc-id: doc-id })
        error-doc-not-found))
    )
    ;; Verify caller is current owner
    (asserts! (document-registered doc-id) error-doc-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) error-not-document-owner)

    ;; Update ownership record
    (map-set real-estate-documents
      { doc-id: doc-id }
      (merge doc-data { owner: new-owner })
    )
    (ok true)
  )
)

;; Delete document from registry
(define-public (delete-document (doc-id uint))
  (let
    (
      (doc-data (unwrap! (map-get? real-estate-documents { doc-id: doc-id })
        error-doc-not-found))
    )
    ;; Ownership verification
    (asserts! (document-registered doc-id) error-doc-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) error-not-document-owner)

    ;; Remove document
    (map-delete real-estate-documents { doc-id: doc-id })
    (ok true)
  )
)

