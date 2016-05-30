# Lunar Language

## 見た目はJavaScriptっぽく

## Fault Tolerant

[Making reliable distributed systems in the presence of software errors](http://ftp.nsysu.edu.tw/FreeBSD/ports/distfiles/erlang/armstrong_thesis_2003.pdf "Making reliable distributed systems in the presence of software errors") の 5 Programming Fault-tolerant Systems (pp.115)を参照。

## 型推論

[Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system "Hindley–Milner type system") を参照。

## クロージャ

関数オブジェクトとして実現。暗黙的にthisをもつ関数。

## 左再帰のParser Combinatorの扱い

[Parser Combinators for Ambiguous Left-Recursive Grammars](http://richard.myweb.cs.uwindsor.ca/PUBLICATIONS/PADL_08.pdf "Parser Combinators for Ambiguous Left-Recursive Grammars")

## Parser Combinator

chain, choice, manyは汎用的な記述方法としてこちら側で定義！

## リアクティブプログラミング

グリーンスレッドはBehavior

## モナド、パイプ、programmable semicolon

```
do parser, p {
    and, ; 継続、bind
    or
    if () {
    }
    
    return true;
} fail {
}
```

```
do socket {
    let fd = socket();
    bind(fd);
}
```