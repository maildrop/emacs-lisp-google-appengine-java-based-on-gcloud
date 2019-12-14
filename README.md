# emacs-lisp-google-appengine-java-based-on-gcloud
Emacs において gcloud ベースの appengine-java を処理するための lisp 

- gcloud-cloud-datastore.el
 gcloud ローカルの cloud-datastore エミュレータを起動及び停止させるための emacs lisp コマンド集

- gcloud-maven.el
 gcloud ベースの appengine 開発環境を起動及び停止させるための emacs lisp コマンド集
 
 この gcloud-maven.el は、Windows上でmvn appengine:run 状態でmvn packageを行うと、jetty が jar ファイルをロックしたままなのでmvn package が失敗する。
 
 このために mvn package を行うために 開発環境を停止させmvn packageして再度開発環境を再開する機能を持つ。
 
