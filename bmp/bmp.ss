(library (bmp)
  (export
   ;; Constructors and Accessors
   make-bmp
   bmp?
   bmp-height
   bmp-width
   bmp-bytevector
   ;; Utilities
   make-bmp-from-path
   make-bmp-from-port
   save-bmp-to-path
   save-bmp-to-port)

  (import (rnrs))

  (define-record-type bmp
    (fields width height bytevector))

  (define-record-type file
    (fields size offset))

  (define-record-type description
    (fields size
	    width
	    height
	    planes
	    bit-count
	    compression
	    size-image
	    x-ppm
	    y-ppm
	    clr-used
	    clr-important
	    red-mask
	    green-mask
	    blue-mask
	    alpha-mask
	    cs-type
	    endpoints
	    gamma-red
	    gamma-green
	    gamma-blue
	    intent
	    profile-data
	    profile-size
	    reserved))



  (define (make-bmp-from-path path)
    (with-input-from-file path
      (lambda () (make-bmp-from-port (current-input-port)))))

  (define (make-bmp-from-port port)
    ;;; Construct an bmp from a file port.
    ;;;
    ;; File port reader utilities.
    (define datum (make-bytevector 4))
    (define (read-u32)
      (get-bytevector-n! port datum 0 4)
      (bytevector-uint-ref datum 0 (endianness little) 4))
    (define (read-s32)
      (get-bytevector-n! port datum 0 4)
      (bytevector-sint-ref datum 0 (endianness little) 4))
    (define (read-u16)
      (get-bytevector-n! port datum 0 2)
      (bytevector-uint-ref datum 0 (endianness little) 2))
    (define (read-s16)
      (get-bytevector-n! port datum 0 2)
      (bytevector-sint-ref
       (get-bytevector-n! port datum 0 2) 0 (endianness little) 2))
    (define (read-u8)
      (get-bytevector-n! port datum 0 1)
      (bytevector-uint-ref datum 0 (endianness little) 1))

 
    (define (read-header port)
      ;;; Read the BMP header from the file port.
      ;;;
      (define head (get-bytevector-n port 14))
      (if (and (= #x42 (bytevector-uint-ref head 0 (endianness little) 1))
	       (= #x4d (bytevector-uint-ref head 1 (endianness little) 1)))
	  (make-file (bytevector-uint-ref head 2 (endianness little) 4)
		     (bytevector-uint-ref head 10 (endianness little) 4))
	  (error 'BMP "Unsupported BMP file.")))


    (define (read-description port)
      ;;; Read the DIB metadata from the file port.
      ;;;
      ;; Determine the descriptor version.
      (define version  (read-u32))
      (define core? (= version 12))
      (define os2x? (or (= version 16) (= version 64)))
      (define info? (= version 40))
      (define ver2? (= version 52))
      (define ver3? (= version 56))
      (define ver4? (= version 108))
      (define ver5? (= version 124))

      ;; Read descriptor into record.
      (define (read-version-5)
	(make-description version (read-s32) (read-s32) (read-u16) (read-u16) (read-u32) (read-u32) (read-s32)
			  (read-s32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32)
			  (list (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32))
			  (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32)))
      (define (read-version-4)
	(make-description version (read-s32) (read-s32) (read-u16) (read-u16) (read-u32) (read-u32) (read-s32)
			  (read-s32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32)
			  (list (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32) (read-u32))
			  (read-u32) (read-u32) (read-u32) #f #f #f #f))
      (define (read-version-info)
	(make-description version (read-s32) (read-s32) (read-u16) (read-u16) (read-u32) (read-u32) (read-s32)
			  (read-s32) (read-u32) (read-u32) #f #f #f #f #f #f #f #f #f #f #f #f #f))
      (define (read-version-core)
	(make-description version (read-u16) (read-u16) (read-u16) (read-u16)
			  0 #f #f #f 0 #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
      (cond (ver5? (read-version-5))
	    (ver4? (read-version-4))
	    (ver3? (error 'BMP "BITMAPV3INFOHEADER unsupported."))
	    (ver2? (error 'BMP "BITMAPV2INFOHEADER unsupported."))
	    (info? (read-version-info))
	    (os2x? (error 'BMP "OS22XBITMAPHEADER unsupported."))
	    (core? (read-version-core))
	    (else
	     (error 'BMP "Unsupported BMP verison." version))))

    
    (define (read-palette port)
      ;;; Read the color table from the file port.
      ;;;
      (error 'BMP "Color tables unimplemented."))


    ;; Read metadata into records.
    (define header (read-header port))
    (define description (read-description port))

    ;; Read color table if any.
    (define palette
      (if (> (description-clr-used description) 0)
	  (read-palette port)
	  #f))


    (define bytevector
      (make-bytevector (* (description-width description) (description-height description) 4)))

    (define (plain)
      ;;; Reads bitmap encoded data into a bmp record.
      ;;;
      (make-bmp (description-width description)
		(description-height description)
		(cond ((= 16 (description-bit-count description))
		       (let read-pixels ((i 0))
			 (if (= i (bytevector-length bytevector)) bytevector
			     (let ((pixel-word (read-u16))
				   (a 255))
			       ;; BGR 565 - other formats exist...
			       (bytevector-u8-set! bytevector (+ i 0) (truncate (* (/ (bitwise-bit-field pixel-word 0 5) 32) 255)))
			       (bytevector-u8-set! bytevector (+ i 1) (truncate (* (/ (bitwise-bit-field pixel-word 5 11) 64) 255)))
			       (bytevector-u8-set! bytevector (+ i 2) (truncate (* (/ (bitwise-bit-field pixel-word 11 16) 32) 255)))
			       (bytevector-u8-set! bytevector (+ i 3) a)
			       (read-pixels (+ 4 i))))))
		      ((= 24 (description-bit-count description))
		       (let read-pixels ((i 0))
			 (if (= i (bytevector-length bytevector)) bytevector
			     (let ((r (read-u8))
				   (g (read-u8))
				   (b (read-u8))
				   (a 255))
			       (bytevector-u8-set! bytevector (+ i 0) b)
			       (bytevector-u8-set! bytevector (+ i 1) g)
			       (bytevector-u8-set! bytevector (+ i 2) r)
			       (bytevector-u8-set! bytevector (+ i 3) a)
			       (read-pixels (+ 4 i))))))
		      ((= 32 (description-bit-count description))
		       (let read-pixels ((i 0))
			 (if (= i (bytevector-length bytevector)) bytevector
			     (let ((r (read-u8))
				   (g (read-u8))
				   (b (read-u8))
				   (a (read-u8)))
			       (bytevector-u8-set! bytevector (+ i 0) b)
			       (bytevector-u8-set! bytevector (+ i 1) g)
			       (bytevector-u8-set! bytevector (+ i 2) r)
			       (bytevector-u8-set! bytevector (+ i 3) a)
			       (read-pixels (+ 4 i))))))
		      (else
		       (error 'BMP "Unsupported pixel bit count." (description-bit-count description))))))


    (define (run-length-encoding-8)
      ;;; Reads RLE8 encoded data into a bmp record.
      ;;;
      (error 'BMP "RLE8 compression unsupported."))


    (define (run-length-encoding-4)
      ;;; Reads RLE4 encoded data into a bmp record.
      ;;;
      (error 'BMP "RLE4 compression unsupported."))

    
    (define (bitfield)
      ;;; Reads bitfield encoded data into a bmp record.
      ;;;
      (define (unmask pixel-word mask)
	(define start (bitwise-bit-count (- (bitwise-and mask (+ (bitwise-not mask) 1)) 1)))
	(define end (+ start (bitwise-bit-count mask)))
	(define value (bitwise-bit-field (bitwise-and pixel-word mask) start end))
	(define length (- (expt 2 (bitwise-bit-count mask)) 1))
	(truncate (* (/ value length) 255)))
      (make-bmp (description-width description)
		(description-height description)
		(cond ((= 16 (description-bit-count description))
		       (let read-pixels ((i 0))
			 (if (= i (bytevector-length bytevector)) bytevector
			     (let ((word (read-u16)))
			       (bytevector-u8-set! bytevector (+ i 0) (unmask word (description-blue-mask description)))
			       (bytevector-u8-set! bytevector (+ i 1) (unmask word (description-green-mask description)))
			       (bytevector-u8-set! bytevector (+ i 2) (unmask word (description-red-mask description)))
			       (bytevector-u8-set! bytevector (+ i 3) (if (> (description-alpha-mask description) 0)
									  (unmask word (description-alpha-mask description))
									  255))
			       (read-pixels (+ 4 i))))))
		      ((= 32 (description-bit-count description))
		       (let read-pixels ((i 0))
			 (if (= i (bytevector-length bytevector)) bytevector
			     (let ((word (read-u32)))
			       (bytevector-u8-set! bytevector (+ i 0) (unmask word (description-blue-mask description)))
			       (bytevector-u8-set! bytevector (+ i 1) (unmask word (description-green-mask description)))
			       (bytevector-u8-set! bytevector (+ i 2) (unmask word (description-red-mask description)))
			       (bytevector-u8-set! bytevector (+ i 3) (if (> (description-alpha-mask description) 0)
									  (unmask word (description-alpha-mask description))
									  255))
			       (read-pixels (+ 4 i))))))
		      (else
		       (error 'BMP "Unsupported pixel bit count." (description-bit-count description))))))


    ;; Determine pixel data encoding.
    (define bmpe? (= (description-compression description) 0))
    (define rle8? (= (description-compression description) 1))
    (define rle4? (= (description-compression description) 2))
    (define bitf? (= (description-compression description) 3))

    (cond (bmpe? (plain))
	  (rle8? (run-length-encoding-8))
	  (rle4? (run-length-encoding-4))
	  (bitf? (bitfield))
	  (else
	   (error 'BMP "Unsupported compression encoding." (description-compression description)))))



  (define (save-bmp-to-path bmp path)
    (error 'BMP "unimplemented" save-bmp-to-path))

  (define (save-bmp-to-port bmp port)
    (error 'BMP "unimplemented" save-bmp-to-port)))
