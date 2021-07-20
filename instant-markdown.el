;;; instant-markdown --- Instant Markdown preview in browser  -*- lexical-binding: t; -*-
;;; Commentary:
;;; SPDX-License-Identifier: Apache-2.0

;;; Code:

(require 'cl-lib)
(require 'web-server)

(defconst im--static "<!doctype html>
<html>
  <head>
    <title>...</title>
    <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css'/>
  </head>
  <body>
    <script defer src='https://cdn.jsdelivr.net/npm/marked@2.0.3/lib/marked.min.js'></script>
    <script defer src='https://cdn.jsdelivr.net/npm/dompurify@2.2.8/dist/purify.min.js'></script>
    <script>
     function loaded() {
         let el = document.getElementById('preview');

         console.log(window.location);
         let s = new WebSocket('ws://'+window.location.host+'/ws'+window.location.pathname+window.location.search);
         s.onopen = ()=>{
             console.log('Established connection to backend.');
         };
         s.onclose = (ev)=>{
             if (ev.wasClean) {
                 console.log('Closed connection to backend.');
             } else {
                 console.log('Uncleanly closed connection to backend.');
             }

         };
         s.onmessage = (ev)=>{
             let data = event.data;
             let rendered = marked(data);
             let safe_rendered = DOMPurify.sanitize(rendered);
             el.innerHTML = safe_rendered;
         };
         s.onerror = (err)=>{
             console.log('Error communicating with backend: '+err.message);
         };
     }

     document.addEventListener('DOMContentLoaded', loaded);
    </script>
    <div style='max-width: 980px' class='markdown-body' id='preview'></div>
  </body>
</html>
")

(defvar im--server
  "Internal variable.")

;; N -> Buffer
(defvar im--buffers (make-hash-table)
  "Internal variable.")

;; N -> connections
(defvar im--connections (make-hash-table)
  "Internal variable.")

(defvar im--next-buffer-n 0
  "Internal variable.")

(defvar im--buffer-n
  "Buffer-local internal variable.")

(defun im--root (request)
  "Serve static root document requested via REQUEST."
  (with-slots (process) request
    (ws-response-header process 200 '("Content-Type" . "text/html"))
    (process-send-string process im--static)))

(defun im--handle-ws (process incoming)
  "Handle (ignore) INCOMING WebSocket message via PROCESS.")

(defun im--change (&optional start end old-len)
  "Send updated Markdown to all clients of the current buffer on change.

START, END and OLD-LEN are required arguments of `after-change-functions'."
  (let ((connections (gethash im--buffer-n im--connections)))
    (dolist (connection connections)
      (if (process-live-p connection)
          (process-send-string connection (ws-web-socket-frame (buffer-string)))
        ;; else
        (puthash im--buffer-n
                 (delq connection (gethash im--buffer-n im--connections))
                 im--connections)))))

(defun im--ws-open (process buffer-n)
  "Handle opened WebSocket connection with PROCESS and number BUFFER-N."
  (puthash buffer-n
           (cons process (gethash buffer-n im--connections))
           im--connections)
  (with-current-buffer (gethash buffer-n im--buffers)
    (im--change))
  :keep-alive)

(defun im--ws-handle (process request buffer-n)
  "Try to upgrade PROCESS connection with HTTP REQUEST for buffer number BUFFER-N to WebSocket."
  (if (ws-web-socket-connect request #'im--handle-ws)
      (im--ws-open process buffer-n)
    ;; else
    (ws-response-header process 400 '("Content-Type" . "text/plain"))
    (process-send-string process "WebSocket connection is expected")))

(defun im--ws (request)
  "Handle HTTP REQUEST for WebSocket endpoint."
  (with-slots (process headers) request
    (let ((path (cdr (assoc :GET headers))))
      (if (string-match "\\`/ws/[1-9][0-9]*\\'" path)
          (im--ws-handle process request (string-to-number (substring path 4)))
        (ws-send-404 process)))))

(defun im--start ()
  "Start `instant-markdown' mode."

  ;; Generate a unique number and store it in buffer-local variable
  (let ((buffer-n im--next-buffer-n))
    (cl-incf im--next-buffer-n)
    (setq-local im--buffer-n buffer-n)

    ;; Start Web server, if it's a first buffer
    (when (= 0 (hash-table-count im--buffers))
      (setq im--server (ws-start
                        '(((:GET . "^/ws") . im--ws)
                          ((:GET . "^/") . im--root))
                        t)))

    ;; Save number->buffer for lookup in HTTP handlers
    (puthash buffer-n (current-buffer) im--buffers)

    ;; Send updates on every change
    ;; 100 means "run after all other change functions"
    (add-hook 'after-change-functions #'im--change 100 t)

    (message "instant-markdown for %s is at http://127.0.0.1:%d/%d"
             (buffer-name)
             (process-contact (ws-process im--server) :service)
             buffer-n)))

(defun im--stop ()
  "Stop `instant-markdown' mode."

  ;; Stop sending updates on changes
  (remove-hook 'after-change-functions #'im--change t)

  ;; Remove this buffer from lookup table
  (remhash im--buffer-n im--buffers)

  ;; Stop Web server, if it was a last buffer
  (when (= 0 (hash-table-count im--buffers))
    (ws-stop im--server)
    (message "instant-markdown HTTP server stopped")))

;;;###autoload
(define-minor-mode instant-markdown-mode
  "Instant Markdown preview mode."
  nil
  " Instant"
  nil
  (if instant-markdown-mode
      (im--start)
    (im--stop)))

(provide 'instant-markdown)
;;; instant-markdown.el ends here
