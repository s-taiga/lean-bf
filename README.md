# bf

Brainf**kの構文定義、処理系実装

# for Windows

mathlibを含むためビルドが失敗する([本家Issue](https://github.com/leanprover/lean4/issues/4159))
そのためビルドする際は以下の手順で行う
1. `lake build`を実行
2. `leanc.exe`の失敗したコマンドの`-o`以降をコピーして`args.txt`に保存
3. `/path/to/leanc.exe @args.txt`
