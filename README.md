# FLOLAC 網站

網站係用 [Hakyll](https://hackage.haskell.org/package/hakyll) 靜態網站編譯器維護，
支援文件轉換器 [Pandoc](https://pandoc.org) 可用 Markdown, LaTeX 甚至 doc 格式。

內容上傳至 GitHub 這個 repo 後，會觸發 [GitHub workflow](/.github/workflows/hakyll.yml)
編譯內容產生網站，並自動上傳到 `gh-pages` 分支。

## 安裝

如需在本地端檢視結果，需先安裝 Haskell 跟 cabal 3.0+。

1. 複製這個 repo：

```
git clone https://github.com/flolac-tw/main_website.git
```

此步驟只需執行一次。

2. 編譯網頁生成器：

接下來在 repo 所在的目錄下執行

```
cabal build
```

之後若網站架構有變才需要重新編譯，原則上只需執行一次。

## 預覽

在 repo 所在的目錄下執行

```
cabal exec -- site watch
```

若成功的話最後會顯示 `Success` 進入監控模式，網站可連入 http://127.0.0.1:8000 瀏覽。
原始檔案變更的時候，會自動重新生成不需要再執行以上指令。

## 上傳變更

原則上產生的網站可直接上傳至 GitHub，但這步請交由設定好的 workflow 執行。

若無權限變更，請發 pull request 由管理者合併變更。

Liang-Ting Chen, 02-02-2020
