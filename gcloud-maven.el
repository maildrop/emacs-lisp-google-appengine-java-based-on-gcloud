;;; gcloud-cloud-datastore.el --- gcloud-cloud-datastore-emulator の起動と停止を行う elisp

;; Copyright (C) 2019 by 精廬幹人

;; Author: TOGURO Mikito <mit@shalab.net>
;; URL: リポジトリの URL等
;; Version: 0.0.1
;; Package-Requires: ()

;; ライセンス

;;; Code:

(require 'xml)
(require 'dom)

(defvar gcloud-maven-last-task "compile")
(defvar gcloud-maven-build-file-name "pom.xml")
(defvar gcloud-maven-command-name "mvn")
(defvar gcloud-maven-appengine-buffer-name "*gcloud-maven-appengine-buffer*")
(defvar gcloud-maven-appengine-process-name "gcloud-maven-appengine-process")


(defun gcloud-maven-find-root ()
  "maven の root ディレクトリを探す"
  (let ((cwd default-directory))
    (locate-dominating-file cwd gcloud-maven-build-file-name)))

(defun gcloud-maven-appengine-webxml-find-project-id ()
  "maven root から src/main/web-app/WEB-INF/appengine-web.xml が存在すればそれを使って application エレメントに書かれた projectId を返す
ファイルもしくはapplication エレメントが存在しない場合には、maven-root の emacs-appengine-config.xml ファイルの application エレメントに書かれた projectId を返す
どちらも、見つからない場合には nil を返す

使い物にならないのでダメ"
  (let ((maven-root (gcloud-maven-find-root)))
    (when maven-root
      (catch 'application
        (dolist (xml-file (list (concat (mapconcat 'file-name-as-directory (list maven-root "src" "main" "webapp" "WEB-INF") "") "appengine-web.xml")
                                (concat (file-name-as-directory maven-root) "emacs-appengine-config.xml")))
          (when (file-exists-p xml-file)
            (let* ((document (with-temp-buffer
                               (insert-file-contents xml-file)
                               (libxml-parse-xml-region (point-min) (point-max))))
                   (element-text (lambda (element)
                                   (when element
                                     (dom-text element))))
               (appid (lambda (document)
                        (when document
                          (funcall element-text (dom-by-tag document 'application)))))
               (projectId (funcall appid document)))
              (when (and projectId
                         (< 0 (length projectId) ))
                (throw 'application projectId )))))))))

(defun gcloud-maven--test-project-id ()
  "テスト用のinteractive関数"
  (interactive)
  (message (format "projectId %s" (gcloud-maven-appengine-webxml-find-project-id))))

(defun gcloud-maven-appengine-still-alivep ()
  ""
  (if (and (get-process gcloud-maven-appengine-process-name)
           (process-live-p (get-process gcloud-maven-appengine-process-name)))
      t
    nil))

(defun gcloud-maven-appengine-stop ()
  ""
  (interactive)
  (when (gcloud-maven-appengine-still-alivep)
    (interrupt-process (get-process gcloud-maven-appengine-process-name)))
  (when (get-process gcloud-maven-appengine-process-name)
    (delete-process (get-process gcloud-maven-appengine-process-name)))
  (remove-hook 'kill-emacs-hook 'gcloud-maven-appengine-stop))

(defun gcloud-maven-appengine-run ()
  ""
  (interactive)
  (let ((maven-root (gcloud-maven-find-root)))
    (when maven-root
      ;; ここで プロセスを始める前に、環境変数の設定をしないといけない。
      (let ( (gcloud-appengine-project-id (gcloud-maven-appengine-webxml-find-project-id))
             (gcloud-datastore-emulator-host "localhost:8081") )
        (if (and gcloud-appengine-project-id
                 gcloud-datastore-emulator-host )
            (progn
              (let* ( (default-directory maven-root) ;; maven ルートを設定
                      (cloud-datastore-environment (list (concat "DATASTORE_DATASET=" gcloud-appengine-project-id)
                                                         (concat "DATASTORE_EMULATOR_HOST=" gcloud-datastore-emulator-host )
                                                         (concat "DATASTORE_EMULATOR_HOST_PATH=" gcloud-datastore-emulator-host "/datastore")
                                                         (concat "DATASTORE_HOST=" "http://" gcloud-datastore-emulator-host )
                                                         (concat "DATASTORE_PROJECT_ID=" gcloud-appengine-project-id)
                                                         "DATASTORE_USE_PROJECT_ID_AS_APP_ID=true"))
                      (process-environment (append cloud-datastore-environment process-environment)) )
                (unless (get-process gcloud-maven-appengine-process-name)
                  (start-process gcloud-maven-appengine-process-name (get-buffer-create gcloud-maven-appengine-buffer-name)
                                 (locate-file gcloud-maven-command-name (parse-colon-path (getenv "PATH")) exec-suffixes) "appengine:run" )
                  ;(set-process-coding-system (get-process gcloud-maven-appengine-process-name) 'utf-8 'utf-8) ; 大本の設定をし直さないとだめっぽい
                  (set-process-query-on-exit-flag (get-process gcloud-maven-appengine-process-name) nil)
                  (add-hook 'kill-emacs-hook 'gcloud-maven-appengine-stop))))
          (message "projectId is not found"))))))

(defun gcloud-maven--compilation-hook-for-restart (buffer message)
  ""
  (message "restarting appengine debug envrioment")
  (gcloud-maven-appengine-run)
  (remove-hook 'compilation-finish-functions 'gcloud-maven--compilation-hook-for-restart))

(defun gcloud-maven ()
  ""
  (interactive)
  (let ((the-project-root (gcloud-maven-find-root))
        (still-alive (gcloud-maven-appengine-still-alivep)))
    (when the-project-root
      (let ((default-directory the-project-root))
        (when still-alive
          (message "stop appengine debug environment and start later")
          (gcloud-maven-appengine-stop)
          (add-hook 'compilation-finish-functions 'gcloud-maven--compilation-hook-for-restart))
        (compile (mapconcat 'identity (list gcloud-maven-command-name "clean" "package") " "))))))

(provide 'gcloud-maven)
;;;
