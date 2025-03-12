;;; bilibili.el --- Watch videos of BiliBili (哔哩哔哩) in org mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/bilibili.el
;; Package-Requires: ((emacs "29.1") (org "9.0") (mpvi "1.0"))
;; Keywords: multimedia, application
;; SPDX-License-Identifier: MIT
;; Version: 0.9

;;; Commentary:
;;
;; Watch videos of BiliBili in org mode.
;;
;; https://bilibili.com is a popular Chinese video website.
;;
;; 在 Emacs 中看 B 站。结合 org + mpvi 食用的一系列辅助方法。
;;
;; 使用步骤:
;;
;;  1. 安装 `mpvi', (use-package bilibili :ensure t)
;;  2. 在 org buffer 中调用 `bilibili-insert-xxx' 插入相关视频
;;  3. 点击链接进行播放
;;
;; ;; https://github.com/SocialSisterYi/bilibili-API-collect

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'mpvi)

(defgroup bilibili nil
  "在 Emacs 中看 B 站。"
  :group 'external
  :prefix 'bilibili-)

(defcustom bilibili-cookie-text nil
  "需要设定的 B 站的 Cookie 文本。
在浏览器 (比如 Chrome/Edge) 打开 bilibili，按 F12，在
Network -> Request Header -> cookie 条目上右键复制可得。"
  :type 'string)

(defcustom bilibili-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "浏览器的 User Agent 值。"
  :type 'string)

(defvar bilibili-myinfo nil)

(defvar url-http-end-of-headers)

(defun bilibili-req (url &optional data headers)
  "请求方法，携带 DATA 和 HEADERS 发送 URL 到服务器。"
  (let ((url-user-agent bilibili-user-agent)
        (url-request-extra-headers `(("content-type" . "application/x-www-form-urlencoded")
                                     ("cookie" . ,bilibili-cookie-text) ,@headers))
        (url-request-method (if data "POST" "GET"))
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t nil)
      (unwind-protect
          (condition-case err
              (progn
                (goto-char url-http-end-of-headers)
                ;; hack for 'space/arc/search': {"code":-509,"message":"请求过于频繁，请稍后再试","ttl":1}{data...}
                (skip-chars-forward " \t\n\r")
                (when (looking-at "{[^}]+} *{")
                  (goto-char (- (match-end 0) 1)))
                (json-read-from-string
                 (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)))
            (error (user-error "Something Wrong: %s" err)))
        (kill-buffer)))))

(defun bilibili-get (url-tpl &rest args)
  "Get 请求，URL-TPL 和 ARGS 跟 `format' 类似。"
  (let ((resp (bilibili-req (apply #'format url-tpl (mapcar (lambda (i) (if (stringp i) (url-hexify-string i) i)) args)))))
    (when (not (= 0 (alist-get 'code resp)))
      (user-error "ERROR %s: %s" (alist-get 'code resp) (alist-get 'message resp)))
    (alist-get 'data resp)))

(cl-defun bilibili-group-number (str &optional (size 4) (char ","))
  "格式化 STR 到指定格式。SIZE 和 CHAR 设定了格式化规则。"
  (unless (stringp str)
    (setq str (format "%s" str)))
  (let ((pt (length str)))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size)) char (substring str (- pt size)))
            pt (- pt size)))
    str))

(defun bilibili-get-padding-spaces (text width &optional limit)
  "获取 TEXT 对齐到 WIDTH 所需要的空格字符，最多 LIMIT 个。"
  (let ((w (string-pixel-width text)) (num 0))
    (while (< w width)
      (setq w (string-pixel-width (concat text (make-string (cl-incf num) ? )))))
    (make-string (max (or limit 0) num) ? )))


;;; Components

(defclass bilibili-video ()
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

(cl-defmethod bilibili-url ((o bilibili-video))
  "返回视频 O 的 URL 地址。"
  (with-slots (bvid) o
    (format "https://www.bilibili.com/video/%s/" bvid)))

(cl-defmethod bilibili-org-item ((o bilibili-video))
  "视频 O 输出到 Org 中的格式。可以根据自己的喜好自定义。"
  (with-slots (author mid title played replied duration date meta) o
    (let* ((time (if (numberp duration) (mpvi-secs-to-hms duration nil t) (string-trim duration)))
           (awidth (if-let* ((ps (and author
                                      (mapcar (lambda (v) (string-pixel-width (slot-value v 'author)))
                                              (alist-get 'videos meta)))))
                       (+ (apply #'max ps) 20)
                     220))
           (space1 (if author (bilibili-get-padding-spaces author awidth 1) ""))
           (twidth (max 1085 (+ (string-pixel-width time) awidth 780)))
           (space2 (bilibili-get-padding-spaces (concat author space1 title time) twidth 1)))
      (format "- %s%s· [[%s][%s]]%s%s | %7s | %9s | %s"
              (if author (format "[[https://space.bilibili.com/%s][%s]]" mid author) "")
              space1 (bilibili-url o) title space2 time
              (if replied (bilibili-group-number replied) "-")
              (if played (bilibili-group-number played) "-")
              (format-time-string "%y-%m-%d" date)))))


;;; APIs

(defun bilibili-get-myinfo ()
  "我的用户信息。"
  (or bilibili-myinfo
      (setq bilibili-myinfo
            (bilibili-get "https://api.bilibili.com/x/space/myinfo"))))

(defun bilibili-get-userinfo (mid)
  "获取用户 MID 的详细信息。"
  (bilibili-get "https://api.bilibili.com/x/space/acc/info?mid=%s" mid))

(defun bilibili-get-videoinfo (bvid)
  "获取视频 BVID 的信息。"
  (bilibili-get "https://api.bilibili.com/x/web-interface/view?bvid=%s" bvid))

(cl-defun bilibili-get-followings (&optional mid (order 'attention))
  "获取所有关注的人，可以通过 MID 和 ORDER 指定用户和排序。"
  (unless mid (setq mid (alist-get 'mid (bilibili-get-myinfo))))
  (unless mid (user-error "MID is required"))
  (let (result total (pn 0))
    (while (or (null total) (< (length result) total))
      (let ((us (bilibili-get "https://api.bilibili.com/x/relation/followings?order_type=%s&vmid=%s&pn=%d&ps=50" (or order "") mid (cl-incf pn))))
        (when (= 0 (length (alist-get 'list us)))
          (user-error "Empty response"))
        (unless total (setq total (alist-get 'total us)))
        (setq result (append result
                             (cl-loop for item across (alist-get 'list us)
                                      collect `((mid . ,(alist-get 'mid item))
                                                (name . ,(alist-get 'uname item))
                                                (sign . ,(alist-get 'sign item))))))))
    result))

(cl-defun bilibili-get-popular (&optional (pn 1) (ps 50))
  "热门视频。可以通过 PN 和 PS 设定数目和页码。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/web-interface/popular?pn=%d&ps=%d" pn ps)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bilibili-video
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

(cl-defun bilibili-get-ranking (&optional (rid 0))
  "排行榜。RID 表示分区号。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/web-interface/ranking/v2?rid=%d" rid)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bilibili-video
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

(cl-defun bilibili-get-precious (&optional (ps 85))
  "入站必刷。PS 表示页数。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/web-interface/popular/precious?page_size=%d" ps)
           for item across (alist-get 'list data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bilibili-video
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

(cl-defun bilibili-get-recommend (&optional (size 10))
  "获取推荐视频。SIZE 表示数目。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/web-interface/index/top/rcmd?version=1&ps=%d" size)
           for item across (alist-get 'item data)
           for stat = (alist-get 'stat item)
           for author = (alist-get 'owner item)
           collect (bilibili-video
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

(defun bilibili-check-fav (rid)
  "检查视频是否被 RID 收藏。"
  (eq (alist-get 'favoured (bilibili-get "http://api.bilibili.com/x/v2/fav/video/favoured?aid=%s" rid)) t))

(defun bilibili-get-user-favs (mid)
  "用户 MID 的收藏夹。"
  (let ((data (bilibili-get "https://api.bilibili.com/x/v3/fav/folder/created/list-all?up_mid=%s" mid)))
    (cl-coerce (alist-get 'list data) 'list)))

(defun bilibili-get-fav-meta (mlid)
  "收藏夹 MLID 的详情。"
  (bilibili-get "https://api.bilibili.com/x/v3/fav/folder/info?media_id=%s" mlid))

(cl-defun bilibili-get-fav-videos (mlid &optional (pn 1) (ps 20))
  "收藏夹 MLID 的内容。PN 和 PS 是分页参数。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/v3/fav/resource/list?media_id=%s&pn=%d&ps=%d" mlid pn ps)
           for item across (alist-get 'medias data)
           for author = (alist-get 'upper item)
           for stat = (alist-get 'cnt_info item)
           collect (bilibili-video
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

(cl-defun bilibili-get-upper-videos (mid &optional (pn 1) (ps 50))
  "用户 MID 发布的视频。PN 和 PS 是分页参数。"
  (let* ((data (bilibili-get "https://api.bilibili.com/x/space/arc/search?mid=%s&pn=%d&ps=%d" mid pn ps))
         (meta (alist-get 'page data)))
    (when (= 0 (length (alist-get 'vlist (alist-get 'list data))))
      (user-error "Empty result"))
    (cl-loop for item across (alist-get 'vlist (alist-get 'list data))
             collect (bilibili-video
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

(cl-defun bilibili-get-upper-season-videos (mid sid &optional (pn 1) (ps 30))
  "用户 MID 创建的合集 SID 的内容。PN 代表页码，PS 代表每页数目。"
  (let* ((data (bilibili-get "https://api.bilibili.com/x/polymer/space/seasons_archives_list?mid=%s&season_id=%s&sort_reverse=false&page_num=%d&page_size=%d" mid sid pn ps))
         (items (alist-get 'archives data)))
    (if (= 0 (length items)) (user-error "Empty response"))
    (let* ((meta (alist-get 'meta data))
           (mid (alist-get 'mid meta))
           (author (alist-get 'name (bilibili-get-userinfo mid))))
      (push `(author . ,author) meta)
      (cl-loop for item across items
               collect (bilibili-video
                        :meta      meta
                        :bvid      (alist-get 'bvid item)
                        :pic       (alist-get 'pic item)
                        :title     (alist-get 'title item)
                        :duration  (alist-get 'duration item)
                        :date      (alist-get 'pubdate item)
                        :mid       mid
                        :author    author
                        :played    (alist-get 'view (alist-get 'stat item)))))))

(cl-defun bilibili-search-videos (keyword &optional (pn 1))
  "用 KEYWORD 搜索视频，PN 代表编码。"
  (cl-loop with data = (bilibili-get "https://api.bilibili.com/x/web-interface/search/type?search_type=video&keyword=%s&page=%d" keyword pn)
           for item across (alist-get 'result data)
           collect (bilibili-video
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

(defun bilibili-fav-video (rid mlid)
  "加入收藏夹. RID is avid or bvid, MLID is media-id, 如果 MLID 为空，那么将是取消收藏。"
  (if (and bilibili-cookie-text (string-match "bili_jct=\\([^;]+\\);" bilibili-cookie-text))
      (let* ((csrf (match-string 1 bilibili-cookie-text))
             (avid (if (string-prefix-p "BV" rid) (alist-get 'aid (bilibili-get-videoinfo rid)) rid))
             (act (if mlid (format "add_media_ids=%s" mlid)
                    (format "del_media_ids=%s"
                            (mapconcat (lambda (f) (format "%s" (alist-get 'id f)))
                                       (bilibili-get-user-favs (alist-get 'mid (bilibili-get-myinfo)))
                                       ","))))
             (params (format "type=2&csrf=%s&rid=%s&%s" csrf avid act))
             (headers `(("origin" . "https://www.bilibili.com")
                        ("referer" . ,(format "https://www.bilibili.com/video/%s/" rid)))))
        (bilibili-req "https://api.bilibili.com/x/v3/fav/resource/deal" params headers))
    (user-error "Cookie invalid")))

(defun bilibili-triple-video (rid)
  "一键三连. RID is avid or bvid。"
  (if (and bilibili-cookie-text (string-match "bili_jct=\\([^;]+\\);" bilibili-cookie-text))
      (let* ((csrf (match-string 1 bilibili-cookie-text))
             (idstr (format "%s=%s" (if (string-prefix-p "BV" rid) "bvid" "aid") rid))
             (params (format "csrf=%s&%s" csrf idstr))
             (headers `(("origin" . "https://www.bilibili.com")
                        ("referer" . ,(format "https://www.bilibili.com/video/%s/" rid)))))
        (bilibili-req "https://api.bilibili.com/x/web-interface/archive/like/triple" params headers))
    (user-error "Cookie invalid")))


;;; Commands

(defmacro bilibili-insert (&rest form)
  "辅助宏，插入内容到 org buffer, FORM 是宏体。"
  `(if (derived-mode-p 'org-mode)
       (save-excursion
         (dolist (v (progn ,@form))
           (insert (bilibili-org-item v) "\n")))
     (user-error "只能插入到 org buffer 中")))

(defun bilibili-update-current-org-items (videos)
  "插入或更新 org buffer 中 VIDEOS 的结果。这个逻辑有待优化。"
  (unless (derived-mode-p 'org-mode)
    (user-error "只能插入到 org buffer 中"))
  (save-restriction
    (save-mark-and-excursion
      (beginning-of-line)
      (org-narrow-to-subtree)
      (let (rs (d "----✄----\n"))
        (dolist (v videos)
          (goto-char (point-min))
          (when (search-forward (format "[%s]" (bilibili-url v)) nil t)
            (beginning-of-line)
            (delete-region (line-beginning-position) (+ 1 (line-end-position))))
          (setf (slot-value v 'meta) `((videos . ,videos) ,@(ignore-errors (slot-value v 'meta))))
          (setq rs (concat rs (bilibili-org-item v) "\n")))
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (skip-chars-forward " \n\t\r")
        (insert rs)
        (if (looking-at-p "[+-] ") (insert d))))))

(defun bilibili-pick-mid ()
  "读取一个 MID 或选择一个关注的人。"
  (let* ((all (cons (bilibili-get-myinfo) (bilibili-get-followings)))
         (items (mapcar (lambda (f) (cons (alist-get 'name f) (alist-get 'mid f))) all))
         (choosen (completing-read "填入 UP 的 mid，或从列表中选择: "
                                   (lambda (input pred action)
                                     (if (eq action 'metadata)
                                         `(metadata (display-sort-function . ,#'identity))
                                       (complete-with-action action items input pred)))))
         (mid (cdr (assoc choosen items))))
    (if (null mid)
        (if (string-match-p "^[0-9]+$" choosen) `((mid . ,choosen))
          (user-error "没找到哦"))
      (cl-find-if (lambda (f) (equal (alist-get 'mid f) mid)) all))))

(defun bilibili-pick-rid ()
  "读取视频的 id, avid 或 bvid, 播放中的优先、光标下的其次。"
  (let* ((node (if (derived-mode-p 'org-mode) (cadr (org-element-context))))
         (path (mpvi-prop 'path)) mid)
    (when (and (null path) (equal "https" (plist-get node :type)))
      (setq path (plist-get node :path)))
    (when (and path (string-match "bilibili.com/video/\\([A-Za-z0-9]+\\)" path))
      (setq mid (match-string 1 path)))
    (let ((mid (read-string "BVID or aid of Video: " mid)))
      (if (string-match-p "^[a-zA-Z0-9]+$" mid) mid
        (user-error "好像输入的不是正确的 aid 或 BVID")))))

(defun bilibili-pick-fav (&optional mid)
  "选中一个收藏夹 MID。"
  (unless mid (setq mid (alist-get 'mid (bilibili-get-myinfo))))
  (unless mid (user-error "MID 不能为空，如果选择自己的收藏夹，请确保 `bilibili-cookie-text' 是正确的"))
  (let* ((favs (bilibili-get-user-favs mid))
         (items (mapcar (lambda (f)
                          (cons (format "%s (%d)"  (alist-get 'title f) (alist-get 'media_count f))
                                (alist-get 'id f)))
                        favs))
         (choosen (completing-read "Fav to choose: "
                                   (lambda (input pred action)
                                     (if (eq action 'metadata)
                                         `(metadata (display-sort-function . ,#'identity))
                                       (complete-with-action action items input pred)))
                                   nil t))
         (id (cdr (assoc choosen items))))
    (cl-find-if (lambda (f) (equal (alist-get 'id f) id)) favs)))

;;;###autoload
(defun bilibili-insert-popular (&optional pn)
  "热门视频，PN 表示页码。"
  (interactive (list (read-number "Page Number: " 1)) org-mode)
  (bilibili-update-current-org-items (bilibili-get-popular (or pn 1))))

;;;###autoload
(defun bilibili-insert-ranking (&optional rid)
  "排行榜，RID 表示分区号。"
  (interactive (list (read-number "分区号: " 0)) org-mode)
  (bilibili-update-current-org-items (bilibili-get-ranking (or rid 0))))

;;;###autoload
(defun bilibili-insert-precious ()
  "入站必刷视频。"
  (interactive nil org-mode)
  (bilibili-update-current-org-items (bilibili-get-precious)))

;;;###autoload
(defun bilibili-insert-recommend ()
  "获取 10 条推荐视频。"
  (interactive nil org-mode)
  (bilibili-update-current-org-items (bilibili-get-recommend)))

;;;###autoload
(defun bilibili-insert-upper-videos (mid pn)
  "UP 主 MID 的所有视频。分页，PN 表示页码，0 表示所有。"
  (interactive (list (if-let* ((id (org-entry-get (point) "MID")))
                         (read-string "mid: " id nil id)
                       (format "%s" (alist-get 'mid (bilibili-pick-mid))))
                     (read-number "Page Number, 0 for all: " 1))
               org-mode)
  (cl-assert (and (string-match-p "^[0-9]+$" mid) (>= pn 0)))
  (let ((vs (if (> pn 0)
                (bilibili-get-upper-videos mid pn)
              (let (result total (page 0))
                (while (or (null total) (< (length result) total))
                  (let ((vs (bilibili-get-upper-videos mid (cl-incf page))))
                    (unless total
                      (setq total (alist-get 'count (slot-value (car vs) 'meta))))
                    (setq result (append result vs))))
                result))))
    (when (> (length vs) 0)
      (bilibili-update-current-org-items vs))
    (org-entry-put (point) "MID" (format "%s" mid))
    (org-entry-put (point) "MEM" (format "UP [%s] 共有 %d 个视频 (%s)"
                                         (slot-value (car vs) 'author)
                                         (alist-get 'count (slot-value (car vs) 'meta))
                                         (format-time-string "%Y/%m/%d")))))

;;;###autoload
(defun bilibili-insert-upper-season-videos (mid sid)
  "UP 主 MID 的某个合集 SID 中的所有视频。"
  (interactive (list (if-let* ((id (org-entry-get (point) "MID")))
                         (read-string "mid: " id nil id)
                       (format "%s" (alist-get 'mid (bilibili-pick-mid))))
                     (if-let* ((id (org-entry-get (point) "SID")))
                         (read-string "season id: " id nil id)
                       (read-string "season id: ")))
               org-mode) ; 没找到合适的列出合集列表的 API
  (cl-assert (and (string-match-p "^[0-9]+$" mid) (string-match-p "^[0-9]+$" mid)))
  (let* ((vs (let (result total (page 0))
               (while (or (null total) (< (length result) total))
                 (let ((vs (bilibili-get-upper-season-videos mid sid (cl-incf page))))
                   (unless total
                     (setq total (alist-get 'total (slot-value (car vs) 'meta))))
                   (setq result (append result vs))))
               result))
         (meta (slot-value (car vs) 'meta)))
    (when (> (length vs) 0)
      (bilibili-update-current-org-items vs))
    (org-entry-put (point) "MID" mid)
    (org-entry-put (point) "SID" sid)
    (org-entry-put (point) "MEM" (format "合集 [%s · %s] 共 %d 个视频 (%s)"
                                         (alist-get 'author meta)
                                         (alist-get 'name meta)
                                         (alist-get 'total meta)
                                         (format-time-string "%Y/%m/%d")))))

;;;###autoload
(defun bilibili-insert-favs (&optional mlid)
  "某个收藏夹下的所有视频，MLID 是收藏夹主键。"
  (interactive (list
                (if-let* ((id (org-entry-get (point) "MEDIA-ID")))
                    (read-string "media-id of fav: " id nil id)
                  (let* ((id (or (org-entry-get (point) "MID")
                                 (ignore-errors (number-to-string (alist-get 'mid (bilibili-get-myinfo))))))
                         (mid (read-string "mid of user: " id nil id))
                         (favs (bilibili-get-user-favs mid))
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
                    (cdr (assoc choosen pairs)))))
               org-mode)
  (let ((meta (bilibili-get-fav-meta mlid)) total (page -1) result)
    (setq total (alist-get 'media_count meta))
    (while (< (length result) total)
      (setq result (append result (bilibili-get-fav-videos mlid (cl-incf page)))))
    (bilibili-update-current-org-items result)
    (org-entry-put (point) "MEDIA-ID" (format "%s" mlid))
    (org-entry-put (point) "MEM" (format "收藏夹 [%s] 共 %d 个视频 (%s)"
                                         (alist-get 'title meta)
                                         (alist-get 'media_count meta)
                                         (format-time-string "%Y/%m/%d")))))

;;;###autoload
(defun bilibili-insert-search (&optional keyword pageno)
  "搜索符合某个 KEYWORD 的视频。PAGENO 表示页码。"
  (interactive (list (read-string "Keyword to search: ")
                     (read-number "Page: " 1))
               org-mode)
  (insert (format "Search '%s', page %d:\n" keyword pageno))
  (bilibili-update-current-org-items (bilibili-search-videos keyword pageno)))

;;; Action

;;;###autoload
(defun bilibili-fav-it ()
  "加入收藏夹。"
  (interactive)
  (let ((rid (bilibili-pick-rid)))
    (if (bilibili-check-fav rid)
        (when (y-or-n-p "本视频已在收藏夹中，是否取消收藏?")
          (bilibili-fav-video rid nil)
          (message "从所有收藏夹取消，结束"))
      (let ((fav (bilibili-pick-fav)))
        (bilibili-fav-video rid (alist-get 'id fav))
        (message "添加到收藏夹 [%s]，成功了" (alist-get 'title fav))))))

;;;###autoload
(defun bilibili-triple-it ()
  "一键三连。"
  (interactive)
  (let ((rid (bilibili-pick-rid)))
    (bilibili-triple-video rid)
    (message "一键三连当前视频，成功了")))


;;; 将 org link 与 `mpvi' 集成。直接点击 bilibili.com 的链接会使用 mpv 打开
;; 如果不需要这个，将 bilibili 从 `mpvi-org-https-link-rules' 移除即可

;;;###autoload
(add-to-list 'mpvi-org-https-link-rules "www.bilibili.com/")

(provide 'bilibili)

;;; bilibili.el ends here
