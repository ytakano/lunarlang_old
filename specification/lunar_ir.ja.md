# Lunar IR

## 所有権

Lunar IRにはオーナーという概念があり、変数を利用する際には、その変数がどのような所有権で扱われるかを指定する。

構文：
- OWNERSHIP := unique | shared | ref

セマンティクス：
- unique
  - ただ唯一のオーナーのみから保持される変数である。所有権の移動はmoveにて行われる。
- shared
  - 複数のオーナーから保持される変数である。内部的には参照カウントを保持している。
- reference
  - unique、shared、immovalbe変数の所有権に影響を及ばさずに参照するための変数である。
- immovable
  - 所有権の変更が許されない変数である。

## 型指定

型指定は、所有権と実際の型を指定して行う。
所有権の指定が省略された場合はimmovable変数となる。

構文：
- TYPE  := TYPE0 | ( OWNERSHIP TYPE0 )
- TYPE0 := SCALAR | VECTOR | STRING | LIST | STRUCT | DICT | SET | DATA | FUNCTYPE | RSTREAM | WSTREAM | IDENTIFIER

ここで、IDENTIFIERとは空白文字以外からなる、1文字以上の文字かつ、先頭が数字ではない文字列かつ、
予約文字（列）以外の文字列である。

## 第一級オブジェクト

### スカラ

以下の型がプリミティブなスカラオブジェクトとして利用可能。
- bool
- u64
- s64
- u32
- s32
- u16
- s16
- u8
- s8
- double
- float
- char (u32の別名、UTF-32）
- atom

構文：
- SCALAR := SCALARTYPE INITSCALAR | SCALARTYPE
- SCALARTYPE := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | binary | char | ATOM
- ATOM := `IDENTIFIER

ただしここで、INITSCALARは数値、真偽値、文字リテラル、atomリテラルのいずれかである。

### 配列

構文：
- VECTOR := ( vector TYPE SIZE )
- SIZE := 0以上の整数

例：
```lisp
(vector u64 10)
(vector (shared u32) 5)
(vector (unique u32) 5)
(vector (ref u32) 5)
```

### 文字列

構文：
- STRING := string

内部的にはUTF-32。

### リスト

構文：
- LIST := ( list TYPE )

例：
```lisp
(list u64)
(list (shared u16))
(list (unique u16))
(list (ref u16))
```

### 構造体

構文：
- STRUCT := ( struct IDENTIFIER ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( struct 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )+ )

例：
```lisp
(struct my_data
  (u32 foo)
  ((shared u32) bar)
  ((unique u32) baz)
  ((ref u32) qux))
```

### 辞書（木）

構文
- DICT := ( dict TYPE TYPE )

セマンティクス
- ( dict Keyの型 Valueの型 )

例：
```lisp
(dict u32 (unique string))
```

### 集合（木）

構文
- SET := ( set TYPE )

セマンティクス
- ( set 値の型 )

```lisp
(set (unique u32))
```

### 多相型

構文：
- DATA := ( data IDENTIFIER ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( data 多相型の名前 ( 多相型となる型 型の名前 )+ )

### 関数型

構文：
- FUNCTYPE := ( func ( TYPE\* ) ( TYPE\* ) )

セマンティクス：
- ( func ( 戻り値の型\* ) ( 引数の型\* ) )

関数には所有権という概念はない。

### ストリーム

ストリームは読み込み用の端点と、書き込み用の端点から構成される。

構文：
- RSTREAM := ( rstrm TYPE )
- WSTREAM := ( wstrm TYPE )

セマンティクス：
- ( rstrm ストリームに渡すデータの型 )
- ( wstrm ストリームに渡すデータの型 )

ストリームには以下の制約がある。
- 読み込み用の端点は所有権がuniqueであり、書き込み用の端点は所有権がsharedでなければならない。
- ストリームが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。

### ポインタ型

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

構文：
- PTR := ( ptr TYPE ) | ( ptr PTR )

セマンティクス：
- ( ptr ポインタの型 )

```lisp
(ptr u64)
(ptr (shared mystruct))
(ptr (ptr (unique u32)))
```

### 共用体

構文：
- UNION := ( union IDENTIFIER ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( union 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )+ )

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

## 関数

### 関数定義

構文：
- FUNC := ( defun IDENTIFIER ( TYPE\* ) ( TYPE IDENTIFIER )\* EXPR\* )

### 関数呼び出し

構文：
- CALLFUNC := ( IDENTIFIER EXPRIDENT\* )
- EXPRIDENT := EXPR | IDENTIFIER

### 無名関数

構文：
- LAMBDA := ( lambda ( TYPE\* ) ( TYPE IDENTIFIER )\* EXPR\* )

## 変数

### 変数生成

- NEW := ( new TYPE )

### 変数束縛

構文：
- LET := ( let ( ( TYPE IDENTIFIER EXPRIDENT )+ ) EXPR\* )

セマンティクス：
- ( let ( ( 束縛 )+ ) 式\* ）
- ( let ( ( 型 変数名 束縛する値 )+ ) 式\* )

### 変数の値書き換え

構文：
- STORE := ( store! EXPRIDENT EXPRIDENT )

セマンティクス：
- ( store! 書き換える変数 書き換える値 )

### 変数の束縛先変更

構文：
- ASSOC := ( assoc! EXPRIDENT EXPRIDENT )

セマンティクス：
- ( assoc! 変数 束縛先 )

ただし、束縛先を変更できるのはuniqueかshared変数のみである。

## 制御式、制御文

### if 式

構文：
- IF := ( if EXPRIDENT EXPRIDENT EXPRIDENT )

セマンティクス：
- ( if 条件 条件が真の時の値 条件が偽の時の値 )

if は式であり値を返す。

### cond 条件分岐

構文：
- COND := ( cond ( EXPRIDENT EXPR\* )+ ?( else EXPR\* ) )

セマンティクス：
- ( cond ( 条件 条件が真の時に実行する式\* )+ ?( else どの条件にも当てはまらない場合に実行する式\* ) )

cond は制御構文であり、値は返さない。

### while ループ

構文：
- WHILE := ( while EXPRIDENT EXPR* )

セマンティクス：
- ( while 条件 条件が真の間実行する式\* )

### break 文

- BREAK := ( break )

while ループの制御から脱出するときに使う。

## 多相型

### type 式

構文：
- TYPEOF := ( type TYPE0 IDENTIFIER )

セマンティクス：
- ( type 検査する形名 識別子 )

type 式は真偽値を返す式であり、多相型変数の型を動的に検査するために利用される。

例：
```lisp
(type u32 var)
```

## ストリーム操作

### ストリームの作成

構文：
- MKSTREAM := ( mkstream TYPE SIZE )

(unique (rstrm TYPE))と(shared (wstrm TYPE))の2つの値を返す。

### push

構文：
- PUSH := ( push! EXPRIDENT )

ストリームの最後尾にデータを挿入する。

### pop

構文：
- POP := ( pop! EXPRIDENT ?SIZE )

セマンティクス：
- ( pop! ストリーム タイムアウト )

ストリームから先頭のデータを取り出す。

## マルチタスキング

### fiber

構文：
- FIBER := ( fiber EXPRIDENT EXPRIDENT* )

セマンティクス：
- ( fiber 呼び出す関数 関数へ渡す引数* )

### yield

構文：
- YIELD := ( yield )

### thread

構文：
- THREAD := ( thread TYPE SIZE EXPRIDENT EXPRIDENT* )

セマンティクス：
- ( thread スレッドキューの型 キューのサイズ 呼び出す関数 関数へ渡す引数* )

ストリームと同じく、スレッドキューには以下の制約がある。
- スレッドキューが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。

## Parser Combinator

## C関数呼び出し

### ccall

構文：
- CCALL := ( ccall IDENTIFIER EXPRIDENT* )

セマンティクス：
- ( ccall C関数名 引数* )

shared、unique変数はポインタ渡し。immovableは値渡しとなる。
pointerのpointerはptr型を利用して実現する。

### dlopen

モジュール読み込み

構文：
- DLOPEN := ( dlopen EXPRIDENT )

セマンティクス：
- ( dlopen モジュールへのパス )

動的ライブラリ、.soファイルを読み込む。

### deref

PTR型の参照外し

構文：
- DEREF := ( deref EXPRIDENT )

セマンティクス：
- ( deref PTR型変数 )

## プリミティブ演算

## リテラル

## ABI (Application Binary Interface)