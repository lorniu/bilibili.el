# Bilibili

在 Emacs 中看 B 站。

一些辅助方法而已。

## 使用步骤

 1. 安装 `mpvi` (https://github.com/lorniu/mpvi)
 2. 下载本包，加入 `load-path`，并加载之
 3. 在 org mode 中调用 `bili-insert-xxx` 插入视频链接
 4. 点击链接播放

## 命令预览

- `bili-insert-recommand` 获取 10 条推荐视频
- `bili-insert-popular/ranking/recommand` 热门视频、排行榜、入站必刷
- `bili-insert-upper-videos` 某 UP 主的所有视频
- `bili-insert-upper-season-videos` 某 UP 主某合集下的所有视频
- `bili-insert-favs` 某收藏夹下所有视频
- `bili-insert-search` 全网搜索

## 补充说明

- 在某个 headline 下，第一次执行上述某命令是插入操作，后续执行同一命令则执行更新操作
- 某些操作需要 Cookie 支持。在浏览器 (比如 Chrome/Edge) 打开 bilibili.com，按 F12，将 Network -> Request Header -> cookie 条目上右键复制的字符串赋值给 `bili-cookie-text` 即可
- 可以通过 `M-x custom-group bili` 查看基本配置选项
- 只是用来刷视频，其他额外 API 暂没对接，以后也大概率没兴趣

## 相关链接

- https://github.com/SocialSisterYi/bilibili-API-collect
