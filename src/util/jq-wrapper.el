;; jq-wrapper.el --- General purpose jq JSON processor interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a general-purpose interface to jq for JSON processing.

(require 'json)
(require 'cl-lib)

(defgroup jq nil
  "Settings for jq command wrapper."
  :group 'tools)

(defcustom jq-executable "jq"
  "Path to the jq executable."
  :type 'string
  :group 'jq)

(defvar jq-debug t
  "When non-nil, print debug information about jq commands.")

(defun jq--debug (format-string &rest args)
  "Print debug message using FORMAT-STRING and ARGS when jq-debug is t."
  (when jq-debug
    (apply #'message (concat "jq-debug: " format-string) args)))

(defun jq--build-command (filter &optional raw stream)
  "Build a jq command string with FILTER.
If RAW is non-nil, add -r flag for raw output.
If STREAM is non-nil, don't wrap in array."
  (let* ((cmd (list jq-executable))
         ;; Add compact output unless streaming
         (cmd (if (not stream)
                  (append cmd '("-c"))
                cmd))
         ;; Add raw output if requested
         (cmd (if raw (append cmd '("-r")) cmd))
         ;; Wrap filter in array unless streaming
         (filter (if stream
                     filter
                   (format "[ %s ]" filter)))
         ;; Add filter
         (cmd (append cmd (list filter))))
    (jq--debug "Built command: %S" cmd)
    cmd))

(defun jq--parse-json (str)
  "Parse STR as JSON, handling errors."
  (condition-case err
      (let ((json-array-type 'list)
            (json-object-type 'hash-table))
        (json-read-from-string str))
    (json-error
     (error "JSON parse error: %s on input: %s"
            (error-message-string err)
            str))))

(defun jq--call-process (input filter raw stream-callback callback)
  "Process INPUT with jq FILTER and handle callbacks.
If RAW is non-nil, don't parse JSON.
STREAM-CALLBACK is called for each line if provided.
CALLBACK is called with final result."
  (jq--debug "Starting jq process with filter: %s" filter)
  (with-temp-buffer
    (let* ((cmd (jq--build-command filter raw (not (null stream-callback))))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8))

      ;; Insert input if provided
      (when input
        (jq--debug "Using input: %s" input)
        (insert input))

      ;; Call jq
      (jq--debug "Calling process with command: %S" cmd)
      (let* ((status (apply #'call-process-region
                            (point-min)
                            (point-max)
                            jq-executable
                            t                ; delete
                            t                ; output to current buffer
                            nil              ; display
                            (cdr cmd)))
             (output (buffer-string)))

        (jq--debug "Process finished with status %s and output: %s"
                   status output)

        ;; Handle streaming output if needed
        (when stream-callback
          (dolist (line (split-string output "\n" t))
            (funcall stream-callback line)))

        ;; Handle final output
        (if (zerop status)
            (when callback
              (condition-case err
                  (let ((result (if raw
                                    output
                                  (jq--parse-json output))))
                    (funcall callback result))
                (error
                 (message "Error in callback: %s" err))))
          (error "jq process failed with status %s: %s"
                 status output))))))

;;;###autoload
(cl-defun jq-parse-sync (input filter &key raw)
  "Synchronously parse INPUT using jq FILTER.
Optional keyword argument :RAW for raw string output."
  (jq--debug "jq-parse-sync called with filter: %s" filter)
  (let (result)
    (jq--call-process input filter raw nil
                      (lambda (r) (setq result r)))
    result))

;;;###autoload
(defun jq-parse (input filter &rest args)
  "Parse INPUT using jq FILTER.
ARGS is a plist of options:
  :callback - Function to call with result
  :raw - Boolean, if true output raw strings
  :stream-callback - Function to call with each line"
  (if-let ((callback (plist-get args :callback)))
      ;; Use async callback
      (jq--call-process input
                        filter
                        (plist-get args :raw)
                        (plist-get args :stream-callback)
                        callback)
    ;; Fall back to sync version if no callback
    (jq-parse-sync input filter
                   :raw (plist-get args :raw))))

;; Filter composition helpers

(defun jq-compose (&rest filters)
  "Compose multiple jq filters into a single pipeline.
Each filter in FILTERS will be properly escaped and joined with |."
  (mapconcat #'identity filters " | "))

(defun jq-map (filter)
  "Create a jq mapping expression using FILTER."
  (format "map(%s)" filter))

(defun jq-select (filter)
  "Create a jq select expression using FILTER."
  (format "select(%s)" filter))

;; Test function
(defun jq-test ()
  "Run some basic tests of jq functionality."
  (let ((jq-debug t))
    ;; Test 1: Simple sync parse
    (message "\nTest 1: Simple sync parse")
    (message "Result: %S"
             (jq-parse-sync "{\"foo\": 42}" ".foo"))

    ;; Test 2: Array processing
    (message "\nTest 2: Array processing")
    (message "Result: %S"
             (jq-parse-sync "[1,2,3,4]" ".[]"))

    ;; Test 3: Async with callback
    (message "\nTest 3: Async with callback")
    (jq-parse "{\"test\": true}" "."
              :callback (lambda (result)
                          (message "Async result: %S" result)))

    ;; Test 4: Stream processing
    (message "\nTest 4: Stream processing")
    (jq-parse "[1,2,3,4]" ".[]"
              :stream-callback (lambda (line)
                                 (message "Stream line: %s" line))
              :callback (lambda (_)
                          (message "Stream processing complete")))))

(defun jq-debug-async-process (process output)
  "Debug function for async process output."
  (with-current-buffer (get-buffer-create "*jq-async-debug*")
    (goto-char (point-max))
    (insert (format "Process %s output:\n%s\n\n" (process-name process) output))))

(defun jq-debug-async-sentinel (process event)
  "Debug function for async process events."
  (with-current-buffer (get-buffer-create "*jq-async-debug*")
    (goto-char (point-max))
    (insert (format "Process %s event: %s\n\n" (process-name process) event))))

(defun jq-debug-async-error-handler (error-symbol error-message error-data)
  "Debug function for handling async errors."
  (with-current-buffer (get-buffer-create "*jq-async-debug*")
    (goto-char (point-max))
    (insert (format "Error: %s\nMessage: %s\nData: %s\n\n"
                    error-symbol error-message error-data))))
(defun jq-run-async-debug (input filter &optional callback)
  "Run jq asynchronously with debugging."
  (let* ((process-name "jq-async-debug")
         (process-buffer (generate-new-buffer "*jq-async-process*"))
         (command `("jq" "-c" ,filter)))
    (with-current-buffer (get-buffer-create "*jq-async-debug*")
      (goto-char (point-max))
      (insert (format "Starting jq-run-async-debug\nInput: %s\nFilter: %s\nCommand: %s\n\n"
                      input filter command)))
    (condition-case err
        (let ((process
               (make-process :name process-name
                             :buffer process-buffer
                             :command command
                             :filter #'jq-debug-async-process
                             :sentinel #'jq-debug-async-sentinel)))
          (process-send-string process input)
          (process-send-eof process)
          (set-process-query-on-exit-flag process nil)
          (with-current-buffer (get-buffer-create "*jq-async-debug*")
            (goto-char (point-max))
            (insert "Process created successfully\n\n")))
      (error
       (with-current-buffer (get-buffer-create "*jq-async-debug*")
         (goto-char (point-max))
         (insert (format "Error creating process: %S\n\n" err)))))))

(defun jq-debug-async-process (process output)
  "Debug function for async process output."
  (with-current-buffer (get-buffer-create "*jq-async-debug*")
    (goto-char (point-max))
    (insert (format "Process %s output:\n%s\n\n" (process-name process) output))))
(defun jq-debug-async-sentinel (process event)
  "Debug function for async process events."
  (with-current-buffer (get-buffer-create "*jq-async-debug*")
    (goto-char (point-max))
    (insert (format "Process %s event: %s\n" (process-name process) event))
    (when (string-match "\\(finished\\|exited\\|failed\\|hangup\\)" event)
      (insert (format "Process has terminated. Exit status: %s\n\n"
                      (process-exit-status process))))))

(provide 'jq-wrapper)
;;; jq-wrapper.el ends here
