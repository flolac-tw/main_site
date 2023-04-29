# FLOLAC 網站 [![Hakyll site CI](https://github.com/flolac-tw/main_site/actions/workflows/hakyll.yml/badge.svg)](https://github.com/flolac-tw/main_site/actions/workflows/hakyll.yml)

網站係用 [Hakyll](https://hackage.haskell.org/package/hakyll) 靜態網站編譯器維護，
支援文件轉換器 [Pandoc](https://pandoc.org) 可用 Markdown, LaTeX 甚至 doc 格式。

* 主要網站：push 至 `master` 後，會觸發 [GitHub workflow](/.github/workflows/hakyll.yml)
編譯內容產生網站，自動發布網站內容到 `flolac-tw/flolac-tw.github.io` 的 `master` 分支更新網站。

* 測試用網站：push 至 `dev` 後會自動生成網站，自動 push 進 `gh-pages` 分支
並發布網站至 `flolac.iis.sinica.edu.tw/main_site/` 

## 安裝

如需在本地端檢視結果，需先安裝 [Stack](https://docs.haskellstack.org/en/stable/README/) 以及 [npm](https://www.npmjs.com)

1. 複製這個 repo：

```
git clone https://github.com/flolac-tw/main_website.git
```

此步驟只需執行一次。

2. 編譯網頁生成器：

接下來在 repo 所在的目錄下執行

```
stack build
```

之後若網站架構有變才需要重新編譯，原則上只需執行一次。

## 預覽結果

在 repo 所在的目錄下執行

```
stack run -- site watch
```

若成功的話最後會顯示 `Success` 進入監控模式，網站可連入 http://127.0.0.1:8000 瀏覽。
原始檔案變更的時候，會自動重新生成不需要再執行以上指令。

## 上傳變更

原則上產生的網站可直接上傳至 GitHub，但請交由設定好的 workflow 執行。

若無權限變更，請發 pull request 由管理者合併變更。
