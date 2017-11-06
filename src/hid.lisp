;;;hid - Communicate with HID devices
;;;Written in 2013 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:hid)

(defclass device ()
  ((handle
    :initarg :handle
    :initform nil
    :accessor handle)
   (ol
    :initform nil
    :accessor ol)))

(cffi:define-foreign-library hid
  (t (:default "hid")))

(cffi:load-foreign-library 'hid)

(cffi:defcstruct hidd-attributes
  (size :int)
  (vendor-id :short)
  (product-id :short)
  (version-number :short))

(cffi:defcfun ("HidD_GetHidGuid" hid_get-hid-guid) :void
  (guid (:pointer (:struct win32:guid))))

(cffi:defcfun ("HidD_GetAttributes" hid_get-attributes) :int
  (device :pointer)
  (attributes (:pointer (:struct hidd-attributes))))

(cffi:defcfun ("HidD_SetOutputReport" hid_set-output-report) :int
  (device :pointer)
  (buf :pointer)
  (length :uint))

(cffi:defcfun ("memset" hid_memset) :pointer
  (dst :pointer)
  (value :uint)
  (count :uint))

#|
Using ReadFile
An application uses the open file handle it obtained by using CreateFile to open a file on the collection. When the application calls ReadFile, it does not have to specify overlapped I/O because the HID Client Drivers buffers reports in a ring buffer. However, an application can use overlapped I/O to have more than one outstanding read request.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Library interface;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-attributes-match-p (handle vid pid)
  (cffi:with-foreign-object (attributes '(:struct hidd-attributes))
    (setf (cffi:foreign-slot-value attributes '(:struct hidd-attributes) 'size)
          (cffi:foreign-type-size '(:struct hidd-attributes)))
    (and
     (not (zerop (hid_get-attributes handle attributes)))
     (= (cffi:foreign-slot-value attributes '(:struct hidd-attributes) 'vendor-id) vid)
     (= (cffi:foreign-slot-value attributes '(:struct hidd-attributes) 'product-id) pid))))

(defun device-matches-p (dev-info di-data vid pid)
  (cffi:with-foreign-objects ((di-detail '(:struct win32:sp-device-interface-detail-data))
			      (size 'win32:dword))
    (setf (cffi:foreign-slot-value di-detail '(:struct win32:sp-device-interface-detail-data) 'win32:size) 5)

    (win32:setup-di-get-device-interface-detail dev-info di-data (cffi:null-pointer) 0 size (cffi:null-pointer))

    (when (zerop (win32:setup-di-get-device-interface-detail
                  dev-info di-data di-detail
                  (cffi:mem-ref size 'win32:dword)
                  size
                  (cffi:null-pointer)))
      (error "SetupDiGetDeviceInterfaceDetail failed"))

    (let ((handle
           (win32:create-file
            (cffi:foreign-slot-pointer
             di-detail
             '(:struct win32:sp-device-interface-detail-data)
             'win32:device-path)
            (logior win32:+generic-read+ win32:+generic-write+)
            (logior win32:+file-share-read+ win32:+file-share-write+)
            (cffi:null-pointer)
            win32:+open-existing+
            win32:+file-flag-overlapped+
            (cffi:null-pointer))))
      (cond
	((handle-attributes-match-p handle vid pid)
	 handle)
	(t
	 (win32:close-handle handle)
	 nil)))))

(defun finalize-handle (handle)
  (win32:close-handle handle))

(defun finalize-ol (ol)
  (win32:close-handle (cffi:foreign-slot-value ol '(:struct win32:overlapped) 'win32:event))
  (cffi:foreign-free ol))

(defmethod initialize-instance :after ((dev device) &key)
  (let ((h (handle dev))
	(event (win32:create-event
		(cffi:null-pointer)
		0
		0
		(cffi:null-pointer)))
	(ol (cffi:foreign-alloc '(:struct win32:overlapped))))

    (trivial-garbage:finalize
     dev
     (lambda ()
       (finalize-handle h)
       (finalize-ol ol)))

    (hid_memset ol 0 (cffi:foreign-type-size 'overlapped))
    (setf (cffi:foreign-slot-value ol '(:struct win32:overlapped) 'win32:event) event
	  (ol dev) ol)))

(defun find-device (dev-info guid vid pid)
  (cffi:with-foreign-object (di-data '(:struct win32:sp-device-interface-data))
    (setf (cffi:foreign-slot-value di-data '(:struct win32:sp-device-interface-data) 'win32:size)
	  (cffi:foreign-type-size '(:struct win32:sp-device-interface-data)))
    (loop :for index :from 0
       :until (zerop (win32:setup-di-enum-device-interface dev-info (cffi:null-pointer) guid index di-data))
       :do
       (let ((handle (device-matches-p dev-info di-data vid pid)))
         (when handle
           (return (make-instance 'device :handle handle)))))))

(defun hid-open (vid pid)
  (cffi:with-foreign-object (guid '(:struct win32:guid))
    (hid_get-hid-guid guid)
    (let ((dev-info
           (win32:setup-di-get-class-devs
            guid
            (cffi:null-pointer)
            (cffi:null-pointer)
            win32:+digcf-deviceinterface+)))

      (unwind-protect
	   (find-device dev-info guid vid pid)
	(win32:setup-di-destroy-device-info-list dev-info)))))

(defun hid-close (device)
  (trivial-garbage:cancel-finalization device)

  (finalize-handle (handle device))
  (finalize-ol (ol device))

  (setf (handle device) nil
	(ol device) nil))

(defun hid-read (device len)
  (when (null (handle device))
    (return-from hid-read nil))

  (cffi:with-foreign-objects ((buf :uint8 len)
			      (read :uint32))
    (hid_memset buf 0 len)
    (win32:reset-event (cffi:foreign-slot-value (ol device) '(:struct win32:overlapped) 'win32:event))
    (when (and (zerop (win32:read-file (handle device) buf len read (ol device)))
	       (/= (win32:get-last-error) win32:+error-io-pending+))
      (win32:cancel-io (handle device))
      (return-from hid-read nil))

    (when (zerop (win32:get-overlapped-result (handle device) (ol device) read 1))
      (return-from hid-read nil))

    (let ((ret (make-array len)))
      (dotimes (i len ret)
	(setf (aref ret i) (cffi:mem-ref buf :uint8 i))))))

(defun hid-write (device buf &aux (len (length buf)))
  (cffi:with-foreign-objects ((ol '(:struct win32:overlapped))
			      (out :uint8 len)
			      (written :uint32))
    (hid_memset ol 0 (cffi:foreign-type-size '(:struct win32:overlapped)))

    (dotimes (i len)
      (setf (cffi:mem-ref out :uint8 i)
	    (elt buf i)))

    (when (and (zerop (win32:write-file (handle device) out len (cffi:null-pointer) ol))
	       (/= (win32:get-last-error) win32:+error-io-pending+))
      (error "WriteFile"))

    (when (zerop (win32:get-overlapped-result (handle device) ol written 1))
      (error "GetOverlappedResult")))
  t)

(defun hid-set-output-report (device buf &aux (len (length buf)))
  (cffi:with-foreign-object (out :uint8 len)
    (dotimes (i len)
      (setf (cffi:mem-ref out :uint8 i)
	    (elt buf i)))

    (/= (hid_set-output-report (handle device) out len) 0)))