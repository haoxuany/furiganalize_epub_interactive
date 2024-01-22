# furiganalize_epub_interactive ("FEI" for short)

This is a complete rewrite of the original [furiganalize-epub](https://github.com/haoxuany/furiganalize-epub) due to my inherent dissatisfaction of how my original code worked. The specific motivations are explained here (TODO: actually write a blog post on this).

## Setup
You will want to grab the [latest release](https://github.com/haoxuany/furiganalize_epub_interactive/releases/) appropriate for your system. So far only MacOS and Debian (and likely most other variants of GNU/Linux) are supported, and I have only tested this on a Mac, so you will need to adapt these instructions according to your system. Let me know if these instructions need to modified!

### Prerequisites
You will need:
- Development headers of zlib installed in your system (due to handling zip files)

Debian/Ubuntu users:
```sh
sudo apt install zlib1g-dev
```

MacOS Users using homebrew:
```sh
brew install zlib
```

If your system isn't listed above take a look at [this setup](https://github.com/ocaml/opam-repository/blob/master/packages/conf-zlib/conf-zlib.1/opam)

- Mecab installed, accessible in PATH, with a valid dictionary (explained below)

- A copy of the generated artifact of [JmdictFurigana](https://github.com/Doublevil/JmdictFurigana) (optional, but very highly recommended)

### Mecab

At the moment the Mecab implementation is not FFI'ed (and this remains a TODO item on my list), so unfortunately for time being you will need to set it up manually.

#### Setting up Mecab

Unix/Linux:
You will need to fetch a copy of the [Mecab source](https://taku910.github.io/mecab/#download).

Unpack, configure, and run make and install.

```sh
./configure
make
sudo make install
```

MacOS:
There is an existing brew formulae that you can use directly:
```sh
brew install mecab
```

#### Setting up a Mecab dictionary
You will need to pick a dictionary to use for Mecab. The official website contains [links](https://taku910.github.io/mecab/#download) to IPA, Juman, and Unidic.

(TODO: add support for custom dictionaries in the future)

Unpack, configure, and run make and install:
```sh
./configure --with-charset=utf8 --enable-utf8-only
make
sudo make install
```

### JmdictFurigana
While not technically required, it is highly recommended that you use a copy of [JmdictFurigana](https://github.com/Doublevil/JmdictFurigana) to help with breaking out Kanji annotations. Otherwise the implementation will default to a hand written generic segmenter that doesn't do very much.

You can obtain the latest release at their [releases](https://github.com/Doublevil/JmdictFurigana/releases) page, and you will likely want the `JmdictFurigana.json` file.

## Usage
The program at the moment runs interactively on the command line. Example usage:
```sh
./fei -i input.epub -o output.epub [-d local.dic] [-f JmdictFurigana.json]
```
where `input.epub` is the path to the epub file you want annotated,
`output.epub` is where the result be written to,
`local.dic` is a "local dictionary" file that will be explained later,
and `JmdictFurigana.json` being the json file obtained from the setup instructions as described above.

After running the command, the program will attempt to annotate the EPUB file. It can also potentially pause halfway and ask for advice on how to handle conflicts. For example something like this:
```
Context:

　たそがれの薄(うす)闇(やみ)に浮かびあがって見えたその看板は、誰の悪戯(いたずら)か、『景』の字に赤いスプレーで×が書かれ、さらに
  望』の字に変えられていた。

What do you do with [悪戯](いたずら)? Mecab Reference: [悪](あく)[戯](ぎ)
[ (k)eep orignal / (kd) keep in dictionary and propagate / defaults to keep ]
[ (m) use mecab / (md) use mecab in dictionary / provide your own correction otherwise ]
```
(Original text taken from [ダンガンロンパ霧切1](https://bookclub.kodansha.co.jp/product?item=0000025549), of which a sample could be read on [Amazon](https://www.amazon.co.jp/%E3%83%80%E3%83%B3%E3%82%AC%E3%83%B3%E3%83%AD%E3%83%B3%E3%83%91%E9%9C%A7%E5%88%87-1-%E6%98%9F%E6%B5%B7%E7%A4%BEFICTIONS-%E5%8C%97%E5%B1%B1-%E7%8C%9B%E9%82%A6/dp/4061388754))

Here, the last two lines of the original text in which the conflict appears are shown, and you are prompted for an action. The conflict in this example is that the original text uses the reading <ruby>悪戯<rt>いたずら</rt></ruby>, whereas Mecab believes that it should be read as <ruby>悪<rt>あく</rt>戯<rt>ぎ</rt></ruby>. The actions you may take are:

- k: Keeps the original text and annotation in the original EPUB file. If you provide an empty input, it will default to k.
- m: Uses the Mecab result instead
- kd: Same as k, but also puts it in the local dictionary and propagates the reading (explained later).
- md: Same as m, but also puts it in the local dictionary and propagates the reading (explained later).
- A specific correction of your own. For example, you may provide `悪[戯](ぎ)`, which will only be annotated as 悪<ruby>戯<rt>ぎ</rt></ruby>. You will then be prompted whether this should be put in the local dictionary as well.

The program will not generate a valid epub file unless all decisions have been made.

## Local Dictionary
A local dictionary file provided through `-d local.dic` are custom annotations and corrections that are valid for the entire book. The format of the file is basically a bunch of annotations that are seperated by a newline.
The reason that this is implemented is to handle custom readings that are more or less impossible to know and that Mecab doesn't get right most of the time.
This is particularly the case for certain expressions that can't be broken down, and names of people.

If a dictionary file is provided to the program, it will run through the entire book and replace any relevant phrases with these annotation, unless it has already been annotated by the book itself.

When running the program interactively and one of the options to put into a local dictionary is provided, it will:
- annotate the rest of the book with the chosen annotation. (Note: it will not go back and annotate anything that came before, you will need to rerun the program).
- add an entry to the `local.dic` file, if it is provided.

Example of what I have for `local.dic` to annotate ダンガンロンパ霧切1:
```
[邦](くに)[猛](たけし)[北山](きたやま)
[無](む)[頓](とん)[着](ちゃく)
[島](しま)[田](だ)[荘](そう)[司](じ)
[島](しま)[田](だ)
[荘](そう)[司](じ)
[御](み)[鏡](かがみ)[霊](れい)
[御](み)[鏡](かがみ)
[明後日](あさって)
[希](き)[望](ぼう)[ヶ](が)[峰](みね)[学](がく)[園](えん)
[埋](うめ)[込](こみ)[灯](とう)
[偏](へん)[執](しつ)[的](てき)
[大](おお)[江](え)[由](よし)[園](ぞの)
[大](おお)[江](え)
[由](よし)[園](ぞの)
[牙](きば)[柳](りゅう)[一](いち)[郎](ろう)
[牙](きば)[柳](りゅう)
[一](いち)[郎](ろう)
[五](ご)[芒](ぼう)[星](せい)
[好](こう)[事](ず)[家](か)
[円](えん)[藤](どう)
[四](よ)[隅](すみ)
[霧](きり)[切](ぎり)[響](きょう)[子](こ)
[霧](きり)[切](ぎり)
[響](きょう)[子](こ)
[切り離](きりはな)
[身体](からだ)
[燕](えん)[尾](び)[椎](しい)[太](た)
[燕](えん)[尾](び)
[椎](しい)[太](た)
[犬](いぬ)[塚](づか)
[甲](こう)
[頰](ほお)
[悪戯](いたずら)
[犬](いぬ)[塚](づか)[甲](こう)
```
As you can see, most of these are names of people that appear in the book, along with some corrections that Mecab simply doesn't get right for some reason.

## Some Less Interesting Notes
- At the moment I'm trying to be as minimally destructive to the original file as possible. However, the order in which the entries that are written back to the zip files have been potentially changed for performance reasons, though this shouldn't cause a problem according to EPUB specs (as mime-type will still be the first file in the entry).
- Only EPUB (OPF) 2.0 and above are supported at the moment.

## Bugs
- There is at the moment one known bug where doing parallel parsing, where apparently Mecab results went out of sync. It's on my list to figure out which cases, since this happens very rarely.
- Some of the code could be written a little bit more efficiently, and in particular some large files need to be streamed rather than read.

## Acknowledgements and License
This implementation depends on these wonderful libraries:
- [Batteries](https://github.com/ocaml-batteries-team/batteries-included) under LGPL 2.1
  + Used everywhere
- [camlzip](https://github.com/xavierleroy/camlzip) under LGPL 2.1 with special exceptions to static linking
  + Used to zip and unzip epub files
- [markup](https://aantron.github.io/markup.ml/) under MIT
  + Used to parse and write xml specifications
- [yojson](https://github.com/ocaml-community/yojson) under BSD
  + Used to parse JmdictFurigana
- [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) under MIT
  + Used for debugging
- [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) under MIT
  + Used to parse JmdictFurigana

The runtime dependencies are:
- [Mecab](https://taku910.github.io/mecab/) under three licenses (GPL, LGPL, BST)
  + Used in a process call for segmenting sentences
- [JmdictFurigana](https://github.com/Doublevil/JmdictFurigana) under an unknown license
  + Used output result for segmenting Kanji readings

This program is distributed under MIT license (or in short, you may do whatever you want as long as the LICENSE file is included in distributions and you don't sue me).

