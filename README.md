[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/bilibili-badge.svg)](https://melpa.org/#/bilibili)

# Bilibili

在 Emacs 中看 B 站 (https://bilibili.com) 视频。

一些辅助方法（命令），配合 [mpvi](https://github.com/lorniu/mpvi) 刷视频、学习很方便。

## 使用步骤

 1. 安装 `mpvi` 包，`(use-package bilibili :ensure t)`
 2. 扫码登录: M-x bilibili-login (某些功能需要登录状态)
 3. 在 org mode 中调用 `bilibili-insert-xxx` 插入视频链接
 4. 点击链接进行视频播放

## 命令预览

- `bilibili-insert-recommend` 获取 10 条推荐视频
- `bilibili-insert-popular/ranking/precious` 热门视频、排行榜、入站必刷
- `bilibili-insert-upper-videos` 某 UP 主的所有视频
- `bilibili-insert-upper-season-videos` 某 UP 主某合集下的所有视频
- `bilibili-insert-favs` 某收藏夹下所有视频
- `bilibili-insert-search` 所有符合关键词的视频
- `bilibili-fav-it/bilibili-triple-it` 加入收藏夹/一键三连

## 补充说明

- 在某个 Headline 下，第一次执行上述某命令是插入操作，后续执行同一命令则执行更新操作
- 有些命令 (比如获取推荐视频) 需要 Cookie 支持，因此需要通过 `bilibili-login` 登录
- 目前只是用来刷视频，其他额外 API 暂没对接

## 相关链接

- https://github.com/SocialSisterYi/bilibili-API-collect
