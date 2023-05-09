;;; bili.el --- BiliBili in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/bili.el
;; Package-Requires: ((emacs "28.1") (mpvi "1") (org "9"))
;; Keywords: multimedia, application
;; SPDX-License-Identifier: MIT
;; Version: 0.9

;;; Commentary:
;;
;; 在 Emacs 中看 B 站。结合 org + mpvi 食用的一系列辅助函数。
;;
;; 使用步骤:
;;
;;  1. 安装 `mpvi'
;;  2. 将本包加入 `load-path' 并加载之
;;  3. 在 org 中调用 `bili-insert-xxx' 插入相关视频
;;  4. 点击链接播放
;;
;; https://github.com/SocialSisterYi/bilibili-API-collect

;;; Code:

(require 'org)
(require 'mpvi)

(defgroup bili nil
  "在 Emacs 中看 B 站."
  :group 'external
  :prefix 'bili-)

(defcustom bili-mid nil
  "你的 mid，是一串数字 (比如 \"2571691\")，可以在浏览器的 url 里看到"
  :type 'string)

(defcustom bili-cookie-text nil
  "在浏览器 (比如 Chrome/Edge) 打开 bilibili，按 F12，在 Network -> Request Header -> cookie 条目上右键复制可得"
  :type 'string)

(defcustom bili-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "浏览器的 User Agent"
  :type 'string)

(defvar url-http-end-of-headers)

(defun bili-req (url &optional data headers)
  (let ((url-debug nil)
        (url-user-agent bili-user-agent)
        (url-request-extra-headers `(("cookie" . ,bili-cookie-text) ,@headers))
        (url-request-method (if data "POST" "GET"))
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t nil)
      (unwind-protect
          (condition-case nil
              (progn
                (goto-char url-http-end-of-headers)
                ;; hack for 'space/arc/search': {"code":-509,"message":"请求过于频繁，请稍后再试","ttl":1}{data...}
                (skip-chars-forward " \t\n\r")
                (when (looking-at "{[^}]+} *{")
                  (goto-char (- (match-end 0) 1)))
                (json-read-from-string
                 (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)))
            (error (user-error "Something wrong")))
        (kill-buffer)))))

(defun bili-get (url-tpl &rest args)
  (let ((resp (bili-req (apply #'format url-tpl (mapcar (lambda (i) (if (stringp i) (url-hexify-string i) i)) args)))))
    (when (not (= 0 (alist-get 'code resp)))
      (user-error "ERROR %s: %s" (alist-get 'code resp) (alist-get 'message resp)))
    (alist-get 'data resp)))

(cl-defun bili-group-number (str &optional (size 4) (char ","))
  (unless (stringp str)
    (setq str (format "%s" str)))
  (let ((pt (length str)))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size)) char (substring str (- pt size)))
            pt (- pt size)))
    str))


;;; Components

(defclass bili-video ()
  ((bvid       :initarg :bvid)
   (pic        :initarg :pic)
   (title      :initarg :title)
   (desc       :initarg :desc)
   (duration   :initarg :duration)
   (date       :initarg :date)
   (tname      :initarg :tname)
   (author     :initarg :author  :initform nil)
   (mid        :initarg :mid :documentation "Author Id")
   (played     :initarg :played  :initform nil)
   (danmaku    :initarg :danmaku :initform nil)
   (replied    :initarg :replied :initform nil)
   (liked      :initarg :liked   :initform nil)
   (coined     :initarg :coined  :initform nil)
   (shared     :initarg :shared  :initform nil)
   (meta       :initarg :meta)))

(cl-defmethod bili-url ((o bili-video))
  (with-slots (bvid) o
    (format "https://www.bilibili.com/video/%s/" bvid)))

(cl-defmethod bili-org-item ((o bili-video))
  "输出到 Org 中的格式。可以根据自己的喜好自定义"
  (with-slots (author title played replied duration date) o
    (format "- %s%s[[%s][%s]]%s  %7s | %6s | %8s | %s"
            (or author "")
            (if author (make-string (max 0 (- 15 (string-width author))) ? ) "")
            (bili-url o) title
            (make-string (max 0 (- 65 (string-width title))) ? )
            (if (numberp duration) (mpvi-secs-to-hms duration nil t) duration)
            (if replied (bili-group-number replied) "-")
            (if played (bili-group-number played) "-")
            (format-time-string "%y-%m-%d" date))))


;;; APIs

(cl-defun bili-get-popular (&optional (pn 1) (ps 50))
  "热门视频"
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/web-interface/popular?pn=%d&ps=%d" pn ps)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'pic item)
                    :title     (alist-get 'title item)
                    :desc      (alist-get 'desc item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubdate item)
                    :tname     (alist-get 'tname item)
                    :author    (alist-get 'name author)
                    :mid       (alist-get 'mid author)
                    :played    (alist-get 'view stat)
                    :danmaku   (alist-get 'danmaku stat)
                    :replied   (alist-get 'reply stat)
                    :liked     (alist-get 'like stat)
                    :coined    (alist-get 'coin stat)
                    :shared    (alist-get 'share stat))))

(cl-defun bili-get-rankings (&optional (rid 0))
  "排行榜"
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/web-interface/ranking/v2?rid=%d" rid)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'pic item)
                    :title     (alist-get 'title item)
                    :desc      (alist-get 'desc item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubdate item)
                    :tname     (alist-get 'tname item)
                    :author    (alist-get 'name author)
                    :mid       (alist-get 'mid author)
                    :played    (alist-get 'view stat)
                    :danmaku   (alist-get 'danmaku stat)
                    :replied   (alist-get 'reply stat)
                    :liked     (alist-get 'like stat)
                    :coined    (alist-get 'coin stat)
                    :shared    (alist-get 'share stat))))

(cl-defun bili-get-precious (&optional (ps 85))
  "入站必刷"
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/web-interface/popular/precious?page_size=%d" ps)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'pic item)
                    :title     (alist-get 'title item)
                    :desc      (alist-get 'desc item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubdate item)
                    :tname     (alist-get 'tname item)
                    :author    (alist-get 'name author)
                    :mid       (alist-get 'mid author)
                    :played    (alist-get 'view stat)
                    :danmaku   (alist-get 'danmaku stat)
                    :replied   (alist-get 'reply stat)
                    :liked     (alist-get 'like stat)
                    :coined    (alist-get 'coin stat)
                    :shared    (alist-get 'share stat))))

(cl-defun bili-get-recommand (&optional (size 10))
  "获取推荐视频"
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/web-interface/index/top/rcmd?version=1&ps=%d" size)
           for item across (alist-get 'item data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'pic item)
                    :title     (alist-get 'title item)
                    :desc      (alist-get 'desc item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubdate item)
                    :tname     (alist-get 'tname item)
                    :author    (alist-get 'name author)
                    :mid       (alist-get 'mid author)
                    :played    (alist-get 'view stat)
                    :danmaku   (alist-get 'danmaku stat)
                    :liked     (alist-get 'like stat))))

(defun bili-get-user-favs (mid)
  "用户的收藏夹"
  ;; (bili-get-user-favs bili-mid)
  (let ((data (bili-get "https://api.bilibili.com/x/v3/fav/folder/created/list-all?up_mid=%s" mid)))
    (cl-coerce (alist-get 'list data) 'list)))

(defun bili-get-fav-meta (mlid)
  "收藏夹详情"
  ;; mlid = fid+mid[:2]
  (bili-get "https://api.bilibili.com/x/v3/fav/folder/info?media_id=%s" mlid))

(cl-defun bili-get-fav-videos (mlid &optional (pn 1) (ps 20))
  "收藏夹内容"
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/v3/fav/resource/list?media_id=%s&pn=%d&ps=%d" mlid pn ps)
           for item across (alist-get 'medias data)
           for author = (alist-get 'upper item)
           for stat = (alist-get 'cnt_info item)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'cover item)
                    :title     (alist-get 'title item)
                    :desc      (alist-get 'intro item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubtime item)
                    :tname     (alist-get 'typename item)
                    :author    (alist-get 'name author)
                    :mid       (alist-get 'mid author)
                    :played    (alist-get 'play stat)
                    :danmaku   (alist-get 'danmaku stat)
                    :replied   (alist-get 'reply stat))
           into rs
           finally (if (= (length rs) 0) (user-error "Empty content") (cl-return rs))))

(cl-defun bili-get-upper-videos (mid &optional (pn 1) (ps 50))
  "用户发布的视频"
  ;; (bili-get-upper-videos "3985676")
  (let* ((data (bili-get "https://api.bilibili.com/x/space/arc/search?mid=%s&pn=%d&ps=%d" mid pn ps))
         (meta (alist-get 'page data)))
    (when (= 0 (length (alist-get 'vlist (alist-get 'list data))))
      (user-error "Empty result"))
    (cl-loop for item across (alist-get 'vlist (alist-get 'list data))
             collect (bili-video
                      :meta      meta
                      :bvid      (alist-get 'bvid item)
                      :pic       (alist-get 'pic item)
                      :title     (alist-get 'title item)
                      :desc      (alist-get 'description item)
                      :duration  (alist-get 'length item)
                      :date      (alist-get 'created item)
                      :tname     (alist-get 'tname item)
                      :author    (alist-get 'author item)
                      :mid       (alist-get 'mid item)
                      :played    (alist-get 'play item)
                      :danmaku   (alist-get 'video_review item)
                      :replied   (alist-get 'comment item)))))

(cl-defun bili-get-upper-season-videos (mid sid &optional (pn 1) (ps 30))
  "用户 MID 创建的合集 SID 的内容"
  ;; (bili-get-upper-season-videos "8047632" "413472")
  (let* ((data (bili-get "https://api.bilibili.com/x/polymer/space/seasons_archives_list?mid=%s&season_id=%s&sort_reverse=false&page_num=%d&page_size=%d" mid sid pn ps))
         (meta (alist-get 'meta data))
         (items (alist-get 'archives data)))
    (if (= 0 (length items)) (user-error "Empty response"))
    (cl-loop for item across items
             collect (bili-video
                      :meta      meta
                      :bvid      (alist-get 'bvid item)
                      :pic       (alist-get 'pic item)
                      :title     (alist-get 'title item)
                      :duration  (alist-get 'duration item)
                      :date      (alist-get 'pubdate item)
                      :mid       (alist-get 'mid meta)
                      :played    (alist-get 'view (alist-get 'stat item))))))

(cl-defun bili-search-videos (keyword &optional (pn 1))
  ;; (bili-search-video "jinitaimei")
  (cl-loop with data = (bili-get "https://api.bilibili.com/x/web-interface/search/type?search_type=video&keyword=%s&page=%d" keyword pn)
           for item across (alist-get 'result data)
           collect (bili-video
                    :bvid      (alist-get 'bvid item)
                    :pic       (alist-get 'pic item)
                    :title     (string-replace "</em>" ""
                                               (string-replace "<em class=\"keyword\">" ""
                                                               (alist-get 'title item)))
                    :desc      (alist-get 'description item)
                    :duration  (alist-get 'duration item)
                    :date      (alist-get 'pubdate item)
                    :tname     (alist-get 'typename item)
                    :author    (alist-get 'author item)
                    :mid       (alist-get 'mid item)
                    :played    (alist-get 'play item)
                    :danmaku   (alist-get 'video_review item))))


;;; Commands

(defmacro bili-insert (&rest form)
  `(if (derived-mode-p 'org-mode)
       (save-excursion
         (cl-loop for video in (progn ,@form)
                  do (insert (bili-org-item video) "\n")))
     (user-error "只能插入到 org buffer 中")))

(defun bili-update-current-org-items (videos)
  "插入或更新 org buffer 中的结果"
  (unless (derived-mode-p 'org-mode)
    (user-error "只能插入到 org buffer 中"))
  (save-restriction
    (save-mark-and-excursion
      (beginning-of-line)
      (org-narrow-to-subtree)
      (let (rs (d "----✄----\n"))
        (dolist (v videos)
          (goto-char (point-min))
          (when (search-forward (format "[%s]" (bili-url v)) nil t)
            (beginning-of-line)
            (delete-region (line-beginning-position) (+ 1 (line-end-position))))
          (setq rs (concat rs (bili-org-item v) "\n")))
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (skip-chars-forward " \n\t\r")
        (insert rs)
        (if (looking-at-p "[+-] ") (insert d))))))

;;;###autoload
(defun bili-insert-popular (&optional pn)
  "热门视频"
  (interactive (list (read-number "Page Number: " 1)))
  (bili-update-current-org-items (bili-get-popular (or pn 1))))

;;;###autoload
(defun bili-insert-ranking (&optional rid)
  "排行榜"
  (interactive (list (read-number "分区号: " 0)))
  (bili-update-current-org-items (bili-get-rankings (or rid 0))))

;;;###autoload
(defun bili-insert-precious ()
  "入站必刷视频"
  (interactive)
  (bili-update-current-org-items (bili-get-precious)))

;;;###autoload
(defun bili-insert-recommand ()
  "获取 10 条推荐视频"
  (interactive)
  (bili-update-current-org-items (bili-get-recommand)))

;;;###autoload
(defun bili-insert-upper-videos (mid pn)
  "某个 UP 主的所有视频。分页，PN 表示页码，0 表示所有"
  (interactive
   (list (if-let (id (org-entry-get (point) "MID"))
             (read-string "mid: " id nil id)
           (read-string "mid of user: "))
         (read-number "Page Number, 0 for all: " 1)))
  (cl-assert (and (string-match-p "^[0-9]+$" mid) (>= pn 0)))
  (let ((vs (if (> pn 0)
                (bili-get-upper-videos mid pn)
              (let (result total (page 0))
                (while (or (null total) (< (length result) total))
                  (let ((vs (bili-get-upper-videos mid (cl-incf page))))
                    (unless total
                      (setq total (alist-get 'count (slot-value (car vs) 'meta))))
                    (setq result (append result vs))))
                result))))
    (when (> (length vs) 0)
      (bili-update-current-org-items vs))
    (org-entry-put (point) "MID" (format "%s" mid))
    (org-entry-put (point) "DESC" (format "UP %s has %d videos"
                                          mid (alist-get 'count (slot-value (car vs) 'meta))))))

;;;###autoload
(defun bili-insert-upper-season-videos (mid sid)
  "某个 UP 主某个合集 SID 中的所有视频"
  (interactive
   (list (if-let (id (org-entry-get (point) "MID"))
             (read-string "mid: " id nil id)
           (read-string "mid of user: "))
         (if-let (id (org-entry-get (point) "SID"))
             (read-string "season id: " id nil id)
           (read-string "season id: "))))
  (cl-assert (and (string-match-p "^[0-9]+$" mid) (string-match-p "^[0-9]+$" mid)))
  (let* ((vs (let (result total (page 0))
               (while (or (null total) (< (length result) total))
                 (let ((vs (bili-get-upper-season-videos mid sid (cl-incf page))))
                   (unless total
                     (setq total (alist-get 'total (slot-value (car vs) 'meta))))
                   (setq result (append result vs))))
               result))
         (meta (slot-value (car vs) 'meta)))
    (when (> (length vs) 0)
      (bili-update-current-org-items vs))
    (org-entry-put (point) "MID" mid)
    (org-entry-put (point) "SID" sid)
    (org-entry-put (point) "DESC" (format "合集·%s (共 %d 个视频)"
                                          (alist-get 'name meta)
                                          (alist-get 'total meta)))))

;;;###autoload
(defun bili-insert-favs (&optional mlid)
  "某个收藏夹下的所有视频，MLID 是收藏夹 id"
  (interactive (list
                (if-let (id (org-entry-get (point) "MEDIA-ID"))
                    (read-string "media-id of fav: " id nil id)
                  (let* ((id (or (org-entry-get (point) "MID") bili-mid))
                         (mid (read-string "mid of user: " id nil id))
                         (favs (bili-get-user-favs mid))
                         (pairs (cl-loop for item in favs
                                         for desc = (format "%s (%d)" (alist-get 'title item) (alist-get 'media_count item))
                                         for mid = (alist-get 'id item)
                                         collect (cons desc mid)))
                         (choosen
                          (completing-read "选择收藏夹: "
                                           (lambda (input pred action)
                                             (if (eq action 'metadata)
                                                 `(metadata (display-sort-function . ,#'identity))
                                               (complete-with-action action pairs input pred)))
                                           nil t)))
                    (org-entry-put (point) "MID" (format "%s" mid))
                    (cdr (assoc choosen pairs))))))
  (let ((meta (bili-get-fav-meta mlid)) total (page -1) result)
    (setq total (alist-get 'media_count meta))
    (while (< (length result) total)
      (setq result (append result (bili-get-fav-videos mlid (cl-incf page)))))
    (bili-update-current-org-items result)
    (org-entry-put (point) "MEDIA-ID" (format "%s" mlid))))

;;;###autoload
(defun bili-insert-search (&optional keyword pageno)
  "搜索符合某个关键词的视频"
  (interactive (list (read-string "Keyword to search: ")
                     (read-number "Page: " 1)))
  (insert (format "Search '%s', page %d:\n" keyword pageno))
  (bili-insert (bili-search-videos keyword pageno)))


;;; 将 org link 与 `mpvi' 集成。直接点击 bilibili.com 的链接会使用 mpv 打开

(defun bili-https-follow-to-mpvi (url arg)
  "让 bilibli 的链接点击后使用 `mpvi-open` 打开"
  (if (and (featurep 'mpvi)
           (string-match-p "bilibili.com/" url))
      (mpvi-open (concat "https:" url))
    (browse-url (concat "https:" url) arg)))

(org-link-set-parameters "https" :follow #'bili-https-follow-to-mpvi)

(provide 'bili)

;;; bili.el ends here
