;;; gcloud-cloud-datastore.el --- gcloud-cloud-datastore-emulator の起動と停止を行う elisp  -*- coding: utf-8 ; lexical-binding: t ;-*-

;; Copyright (C) 2019 by TOGURO Mikito

;; Author: TOGURO Mikito <mit@shalab.net>
;; URL: リポジトリの URL等
;; Version: 0.0.1
;; Package-Requires: ()

;; ライセンス

;;; Code:

(defvar gcloud-cloud-datastore-emulator-process-name "gcloud-cloud-datastore-emulator"
  "gcloud-cloud-datastore-emulator のプロセス名" )
(defvar gcloud-cloud-datastore-emulator-buffer-name "*gcloud-cloud-datastore-emulator*"
  "gcloud-cloud-datastore-emulator のバッファ名" )

(defun gcloud-cloud-datastore-emulator-remove-process ()
  "gcloud cloud-datastore のエミュレータのプロセスを除去します"
  (when (get-process gcloud-cloud-datastore-emulator-process-name)
    (when (process-live-p (get-process gcloud-cloud-datastore-emulator-process-name) )
      (interrupt-process (get-process gcloud-cloud-datastore-emulator-process-name)))
    (delete-process (get-process gcloud-cloud-datastore-emulator-process-name))
  (remove-hook 'kill-emacs-hook 'gcloud-cloud-datastore-emulator-remove-process)))

(defun gcloud-cloud-datastore-emulator-start-impl ()
  "gcloud cloud-datastore エミュレータの動作を実際に始めます"
  (unless (and (get-process gcloud-cloud-datastore-emulator-process-name)
               (process-live-p (get-process gcloud-cloud-datastore-emulator-process-name)))
    (let ( (process-connection-type nil) )
      (start-process gcloud-cloud-datastore-emulator-process-name
                     (get-buffer-create gcloud-cloud-datastore-emulator-buffer-name)
                     "gcloud.cmd" "beta" "emulators" "datastore" "start" )
      (set-process-query-on-exit-flag (get-process gcloud-cloud-datastore-emulator-process-name) nil)
      (add-hook 'kill-emacs-hook 'gcloud-cloud-datastore-emulator-remove-process))))

(defun gcloud-cloud-datastore-emulator-start ()
  "gcloud cloud-datastore のエミュレータの動作を始めます。

この関数は、interactive用にバッファの表示を行います"
  (interactive)
  (let ((the-buffer (get-buffer-create gcloud-cloud-datastore-emulator-buffer-name)))
    (display-buffer the-buffer)
    (with-current-buffer the-buffer
      (goto-char (point-max))))
  (gcloud-cloud-datastore-emulator-start-impl))

(defun gcloud-cloud-datastore-emulator-stop ()
  "gcloud ccloud-datastore のエミューレタの動作を停止します。"
  (interactive)
  (gcloud-cloud-datastore-emulator-remove-process))

(provide 'gcloud-cloud-datastore )

;;; ファイル名.el ends here
