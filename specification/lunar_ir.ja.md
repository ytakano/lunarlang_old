# Lunar IR

Lunar言語の中間表現であり、ここからLLVM IRへ変換。

# 構文

- IR           := TOP*
- TOP          := FUNC | GLOBAL | THREADLOCAL | IMPORT | EXPR | TOPSTATEMENT
- TOPSTATEMENT := LET | COND | WHILE | SELECT | STRUCT | CUNION | UNION
- STATEMENT    := LET | COND | WHILE | BREAK | SELECT | RETURN | STRUCT | CUNION | UNION | BLOCK | LEAP
- STEXPR       := STATMENT | EXPR
- LITERAL      := STR32 | STR8 | CHAR32 | CHAR8 | INT | FLOAT | HEX | OCT | BIN | ATOM
- EXPRIDENT    := EXPR | IDENTIFIER
- EXPRIDENTLIT := EXPR | IDENTIFIER | LITERAL
- EXPR         := CALLFUNC

# グローバル変数定義

- GLOBAL := ( global ( ( TYPE ( IDENTIFIER+ ) EXPRIDENTLIT )+ ) )

# スレッドローカル変数定義

- THREADLOCAL := ( threadlocal ( ( TYPE ( IDENTIFIER+ ) EXPRIDENTLIT )+ ) )

# インポート

- IMPORT := ( import STR32+ )

# 所有権

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

# 型指定

型指定は、所有権と実際の型を指定して行う。
所有権の指定が省略された場合はimmovable変数となる。

構文：
- TYPE  := TYPE0 | ( OWNERSHIP TYPE0 )
- TYPE0 := SCALAR | ARRAY | STRING | BINARY | LIST | STRUCT | DICT | SET | UNION | FUNCTYPE | RSTREAM | WSTREAM | RFILESTREAM | WFILESTREAM | RSOCKSTREAM | WSOCKSTREAM | RSIGSTREAM | RTHREADSTREAM | WTHREADSTREAM | PTR | CUNION | PARSEC | IDENTIFIER

ここで、IDENTIFIERとは空白文字以外からなる、1文字以上の文字かつ、先頭が数字ではない文字列かつ、
予約文字（列）以外の文字列である。

# 第一級オブジェクト

## スカラ

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
- SCALAR := SCALARTYPE LITERAL | SCALARTYPE
- SCALARTYPE := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | char | atom

## 関数型

構文：
- FUNCTYPE := ( func ( TYPE\* ) ( TYPE\* ) )

セマンティクス：
- ( func ( 戻り値の型\* ) ( 引数の型\* ) )

関数には所有権という概念はない。

## 構造体

構文：
- STRUCT := ( struct IDENTIFIER? ( TYPE IDENTIFIER )* )

セマンティクス：
- ( struct 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )* )

## 多相型（直和集合）

構文：
- UNION := ( union IDENTIFIER? ( TYPE IDENTIFIER )* )

セマンティクス：
- ( union 多相型の名前 ( 多相型となる型 型の名前 )* )

## C共用体

構文：
- CUNION := ( cunion IDENTIFIER? ( TYPE IDENTIFIER )* )

セマンティクス：
- ( cunion 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )* )

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

## ポインタ型

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

構文：
- PTR := ( ptr TYPE )

セマンティクス：
- ( ptr ポインタの型 )

```lisp
(ptr u64)
(ptr (shared mystruct))
(ptr (ptr (unique u32)))
```

# 第二級オブジェクト

## 配列

構文：
- ARRAY := ( array TYPE SIZE ) | ( array TYPE )
- SIZE  := DIGIT | HEX | OCT | BIN

SIZEを指定した場合は、固定長となる。

例：
```lisp
(array u64 10)
(array (shared u32) 5)
(array (unique u32) 5)
(array (ref u32) 5)
```

## 文字列

構文：
- STRING := string

内部的にはUTF-32。

## バイナリ列

構文：
- BINARY := binary

## リスト

構文：
- LIST := ( list TYPE )

例：
```lisp
(list u64)
(list (shared u16))
(list (unique u16))
(list (ref u16))
```

例：
```lisp
(struct my_data
  (u32 foo)
  ((shared u32) bar)
  ((unique u32) baz)
  ((ref u32) qux))
```

## 辞書（木）

構文
- DICT := ( dict TYPE TYPE )

セマンティクス
- ( dict Keyの型 Valueの型 )

例：
```lisp
(dict u32 (unique string))
```

## 集合（木）

構文
- SET := ( set TYPE )

セマンティクス
- ( set 値の型 )

```lisp
(set (unique u32))
```

### ストリーム型

### データストリーム型

データストリームは読み込み用の端点と、書き込み用の端点から構成される。

構文：
- RSTREAM := ( rstrm TYPE )
- WSTREAM := ( wstrm TYPE )

セマンティクス：
- ( rstrm ストリームに渡すデータの型 )
- ( wstrm ストリームに渡すデータの型 )

データストリームには以下の制約がある。
- 読み込み用の端点は所有権がuniqueであり、書き込み用の端点は所有権がsharedでなければならない。
- データストリームが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。
- データストリームに送信する構造体内にある変数は、shared、unique、immovable変数のみである。ref変数を内部に持っている構造体は、ストリームで送信することは出来ない。

### ファイルストリーム型

- RFILESTREAM := rfilestrm
- WFILESTREAM := wfilestrm

### ソケットストリーム型

- RSOCKSTREAM := rsockstrm
- WSOCKSTREAM := wsockstrm

### シグナルストリーム型

- RSIGSTREAM  := rsigstrm

### スレッドデータストリーム型

- RTHREADTREAM := ( rthreadstrm TYPE )
- WTHREADTREAM := ( wthreadstrm TYPE )

## パーサ型

構文：
- PARSEC := ( parsec string ) | ( parsec binary)

# 関数

## 関数定義構文

構文：
- FUNC := ( defun IDENTIFIER ( TYPE\* ) ( TYPE IDENTIFIER )\* STEXPR\* )

## 関数呼び出し式

構文：
- CALLFUNC := ( EXPRIDENT EXPRIDENTLIT\* )

関数定義に応じた値を返す。

## 無名関数定義式

構文：
- LAMBDA := ( lambda ( TYPE\* ) ( TYPE IDENTIFIER )\* STEXPR\* )

関数型の値を返す。

# 構文

## 変数束縛構文

構文：
- LET := ( let ( ( ( ( TYPE IDENTIFIER )+ ) EXPRIDENTLIT )+ ) STEXPR\* )

セマンティクス：
- ( let ( ( ( ( 型 変数名 )+ ) 束縛する値 )+ ) 式\* )

関数は複数の値を返すこともあるため、複数の変数名を記述できるように。

## if 式

構文：
- IF := ( if EXPRIDENTLIT EXPRIDENTLIT EXPRIDENTLIT )

セマンティクス：
- ( if 条件 条件が真の時の値 条件が偽の時の値 )

if は式であり値を返す。C言語の?構文みたいなもの。

## cond 条件分岐構文

構文：
- COND := ( cond ( EXPRIDENTLIT STEXPR\* )+ ( else STEXPR\* )? )

セマンティクス：
- ( cond ( 条件 条件が真の時に実行する式\* )+ ( else どの条件にも当てはまらない場合に実行する式\* )? )

cond は制御構文であり、値は返さない。

## while ループ文

構文：
- WHILE := ( while EXPRIDENTLIT STEXPR* )

セマンティクス：
- ( while 条件 条件が真の間実行する式\* )

## break 文

構文：
- BREAK := ( break )

while ループの制御から脱出するときに使う。

## block 文

構文：
- BLOCK := ( block STEXPR* )

セマンティクス：
- ( block 式\* )

## leap 文

構文：
- LEAP := ( leap )

block 脱出するときに使う。

## return 文

構文：
- RETURN := ( return EXPRIDENTLIT* )

セマンティクス：
- ( return 返り値* )

## select 文

構文：
- SELECT := ( select ( EXPRIDENT STEXPR\* )\* ( timeout EXPRIDENTLIT STEXPR* )? )

セマンティクス：
- ( select (ストリーム ストリームに入力があった時に実行する式) (timeout タイムアウトするまでの時間[ms] )? )

ストリームの入力待ちを行う。
入力待ちの際、他に実行可能なグリーンスレッドがある場合はそちらに処理が移行。

# リテラル

## atom

- ATOM := `IDENTIFIER

## 文字列

- STR32  := " CHARS* "
- STR8   := b " CHARS* "
- ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\\\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
- CHARS  := ESCAPE | ESCAPE以外の文字

## 文字

- CHAR32 := ' CHARS '
- CHAR8  := b ' CHARS '

## 整数

- INT     := -? DIGIT
- DIGIT   := NUM1to9 NUM0to9* | 0
- NUM1to9 := 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
- NUM0to9 := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

## 浮動小数

- FLOAT := INT . NUM0to9+ EXP? f?
- EXP   := EE SIGN NUM+
- EE    := e | E
- SIGN  := - | +

最後にfがついた場合は単精度で、つかない場合は倍精度となる。

## 16進数

- HEX     := 0x HEXNUM2\* | 0X HEXNUM2\*
- HEXNUM2 := HEXNUM HEXNUM
- HEXNUM  := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | A | b | B | c | C | d | D | f | F

## 8進数

- OCT    := 0 OCTNUM*
- OCTNUM := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

## 2進数

- BIN    := 0b BINNUM\* | 0B BINNUM\*
- BINNUM := 0 | 1

## 真偽値

- TRUE  := ture
- FALSE := false

# 標準関数

## 変数

### 変数生成式

構文：
- NEW := ( new TYPE EXPRIDENTLIT* )

セマンティクス：
- ( new 型 初期化引数* )

TYPE型の値を返す。

### 変数の値書き換え式

構文：
- COPY := ( copy EXPRIDENT EXPRIDENTLIT )

セマンティクス：
- ( copy 書き換える変数 書き換える値 )

### 変数の束縛先変更式

構文：
- ASSOC := ( assoc EXPRIDENT EXPRIDENT )

セマンティクス：
- ( assoc 変数 束縛先 )

ただし、束縛先を変更できるのはuniqueかshared変数のみである。

## type 式、型検査

構文：
- TYPEOF := ( type TYPE0 EXPRIDENTLIT )

セマンティクス：
- ( type 検査する形名 識別子など )

type 式は真偽値を返す式であり、多相型変数の型を動的に検査するために利用される。

例：
```lisp
(type u32 var)
```

## ストリーム

### ストリーム生成式

構文：
- MKSTREAM := ( mkstream TYPE EXPRIDENTLIT )

セマンティクス：
- ( mkstream ストリームの型 サイズ )

(unique (rstrm TYPE))と(shared (wstrm TYPE))の2つの値を返す。

### ファイルストリーム生成式

構文：
- MKSTREAM := ( mkfilestream EXPRIDENTLIT )

セマンティクス：
- ( mkfilestream ファイルディスクリプタ )

### ソケットストリーム生成式

構文：
- MKSOCKSTREAM := ( mksockstream EXPRIDENTLIT )

セマンティクス：
- ( mksockstream ソケットファイルディスクリプタ )

### シグナルストリーム生成式

構文：
- MKSIGNALSTREAM := ( mksognalstream EXPRIDENTLIT )

セマンティクス：
- ( mksignalstream シグナル番号 )

### push式

構文：
- PUSH := ( push EXPRIDENTLIT EXPRIDENTLIT )

セマンティクス：
- ( push ストリーム型 挿入するデータ )

ストリームの最後尾にデータを挿入する。
STRM_SUCCESS, STRM_CLOSED, STRM_NO_VACANCYのいずれかの値を返す。

### pop式

構文：
- POP := ( pop EXPRIDENT )

セマンティクス：
- ( pop ストリーム )

ストリームから先頭のデータを取り出す。
返り値は、(取り出した値 エラー)となり、エラーはSTRM_SUCCESS, STRM_CLOSED,
STRM_NO_VACANCYのいずれかとなる。

## マルチタスキング

### spawn式

構文：
- SPAWN := ( spawn EXPRIDENTLIT EXPRIDENT EXPRIDENTLIT)

セマンティクス：
- ( spawn スタックサイズ 呼び出す関数 関数へ渡す引数)

返り値はスレッド内で一意に識別されるs64型の整数値。

### schedule式

構文：
- SCHEDULE := ( schedule )

他のグリーンスレッドに制御を渡す。

### thread式

OSネイティブなデタッチスレッドを生成。

構文：
- THREAD := ( thread EXPRIDENTLIT TYPE EXPRIDENTLIT EXPRIDENT EXPRIDENTLIT )

セマンティクス：
- ( thread スレッドの名前 スレッドキューの型 キューのサイズ 呼び出す関数 関数へ渡す引数* )

ストリームと同じく、スレッドキューには以下の制約がある。
- スレッドキューが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。

## ロック・同期処理

### spin_lock式

- SPIN_LOCK := ( spin_lock EXPRIDENT )

### spin_lock_init式

- SPIN_LOCK_INIT := ( spin_lock_init EXPRIDENT )

### spin_try_lock式

- SPIN_TRY_LOCK := ( spin_try_lock EXPRIDENT )

### spin_unlock式

- SPIN_UNLOCK := ( spin_unlock EXPRIDENT )

## Parser Combinator

- PARSE        := (parse EXPRIDENT PARSECOPS EXPRIDENTLIT*)
- PARSECOPS    := PARSECCHAR | PARSECMANY | PARSECMANY1 | PARSECTRY | PARSECTRYEND | PARSECLA | PARSECLAEND | PARSECDIGIT | PARSECHEX | PARSECOCT | PARSECSPACE | PARSECSATIS | PARSECSTR
- PARSECCHAR   := character
- PARSECTRY    := try
- PARSERTRYEND := try_end
- PARSECLA     := look_ahead
- PARSECLAEND  := look_ahead_end
- PARSECDIGT   := digit
- PARSECHEX    := hex
- PARSECOCT    := oct
- PARSECSPACE  := space
- PARSECSATIS  := satisfy
- PARSECSTR    := string
- PARSECRESULT := result

```lisp
(let (parsec (p) (new parsec string rstream))
  (parse p character 'a'))
```

## C関数呼び出し

### ccall

構文：
- CCALL := ( ccall EXPRIDENT EXPRIDENTLIT* )

セマンティクス：
- ( ccall C関数 引数* )

shared、unique変数はポインタ渡し。immovableは値渡しとなる。
pointerのpointerはptr型を利用して実現する。

### dlopen式

モジュール読み込み

構文：
- DLOPEN := ( dlopen EXPRIDENTLIT )

セマンティクス：
- ( dlopen モジュールへのパス )

動的ライブラリ、.soファイルを読み込む。

返り値はハンドラ。

### dlclose式

- DLCLOSE := ( dlclose EXPRIDENT )

### dlsym式

構文：
- DLSYM := ( dlsym EXPRIDENT EXPRIDENTLIT )

セマンティクス：
- ( dlsym モジュール シンボル名 )

### toptr式

PTR型へ変換

構文：
- TOPTR := ( toptr EXPRIDENT )

### deref式

PTR型の参照外し

構文：
- DEREF := ( deref EXPRIDENT )

セマンティクス：
- ( deref PTR型変数 )

返り値は、参照外しを行った値。

## 参照カウント

### inccnt式

- INCCNT := ( inccnt EXPRIDENT )

### deccnt式

- DECCNT := ( deccnt EXPRIDENT )

## プリミティブ演算

### 四則演算・剰余算

- ADD   := ( add EXPRIDENTLIT EXPRIDENTLIT+ )
- MINUS := ( minus EXPRIDENTLIT EXPRIDENTLIT+ )
- MULTI := ( mul EXPRIDENTLIT EXPRIDENTLIT+ )
- DIV   := ( div EXPRIDENTLIT EXPRIDENTLIT+ )
- MOD   := ( mod EXPRIDENTLIT EXPRIDENTLIT+ )

### ビット演算

- BAND := ( band EXPRIDENTLIT EXPRIDENTLIT+ )
- BOR  := ( bor EXPRIDENTLIT EXPRIDENTLIT+ )
- BXOR := ( bxor EXPRIDENTLIT EXPRIDENTLIT+ )
- BNOT := ( bnot EXPRIDENTLIT )
- BSL  := ( bsl EXPRIDENT EXPRIDENT )  // 論理左シフト
- BSR  := ( bsr EXPRIDENT EXPRIDENT )  // 論理右シフト
- BASL := ( basl EXPRIDENT EXPRIDENT ) // 算術左シフト
- BASR := ( basr EXPRIDENT EXPRIDENT ) // 算術右シフト
- BPOPCNT := ( bpopcnt EXPRIDENT )
- BLZCNT  := ( blzcnt EXPRIDENT )

### 論理演算

- AND := ( and EXPRIDENTLIT EXPRIDENTLIT+ )
- OR  := ( or EXPRIDENTLIT EXPRIDENTLIT+ )
- EQ  := ( eq EXPRIDENTLIT EXPRIDENTLIT+ )
- NOT := ( not EXPRIDENTLIT )

## IO

### socket式

- SOCKET     := ( socket SOCKDOMAIN SOCKTYPE )
- SOCKDOMAIN := PF_UNIX | PF_INET | PF_INET6
- SOCKTYPE   := SOCK_STREAM | SOCK_DGRAM | SOCK_RAW

返り値のディスクリプタはmksockstreamと結び付けられなければならない。

### open式

構文：
- OPEN   := ( open EXPRIDENTLIT (OFLAGS*) )
- OFLAGS := O_RDONLY | O_WONLY | O_RDWR | O_APPEND | O_CREAT | O_TRUNC | O_EXCL | O_SHLOCK | O_EXLOCK | O_NOFOLLOW | O_SYMLINK | O_EVTONLY | O_CLOEXEC

セマンティクス：
- ( open フィアル名 フラグ )

返り値のディスクリプタはmkfilestreamと結び付けられなければならない。

### print式

- PRINT := ( print EXPRIDENTLIT+ )

文字列を標準出力へ出力。引数はstring型のみ。

## 文字列変換式

- TOSTR := ( tostr EXPRIDENTLIT )

引数を文字列へ変換。immovalなstring型を返す。

# Application Binary Interface (ABI)