COBOL.vim
=========

Vim runtime files for COBOL programming language

## Installation
### [minPlug](https://github.com/Jorengarenar/minPlug):
```
MinPlug Jorengarenar/COBOl.vim
```

### [vim-plug](https://github.com/junegunn/vim-plug):
```vim
Plug 'Jorengarenar/COBOl.vim'
```

### Vim's packages
```bash
mkdir -p ~/.vim/pack/plugins/start
cd ~/.vim/pack/plugins/start
git clone https://github.com/Jorengarenar/COBOl.vim.git
```

### [NeoBundle](https://github.com/Shougo/neobundle.vim)
```vim
NeoBundle 'Jorengarenar/COBOl.vim'
```

## Configuration

Global (or buffer) variables:

|    variable (`g:`/`b:`)   |   def   | description |
|---------------------------|---------|-------------|
| `cobol_legacy_code`       |   `0`   | set to `1` for legacy mode
| `cobol_colorcolumns`      |   `0`   | set to `1` for borders between areas
| `cobol_folding`           |   `0`   | set to `1` for syntax based code folding
| `cobol_autoupper`         |   `0`   | set to `1` to automatically change keywords to uppercase
| `cobol_indent_data_items` |   `1`   | indentation of items in `DATA DIVISION` (`0` - all in area A, `1` - to area B, `2` - each level more indented)
| `cobol_indent_id_paras`   |   `0`   | indent paragraphs in `IDENTIFICATION DIVISION` to area B
| `cobol_indent_paras_B`    |   `0`   | indent all paragraphs declarations to area B
| `cobol_comp_mp_cobc`      |   `0`   | if set to `1`, `compiler` will set `cobc\ -O\ -x` as `makeprg`
| `cobol_comp_mp`           |  `""`   | if not empty and `cobol_comp_mp_cobc` not set to `1`, the value will be used to set `makeprg`

---

Contains updated version of [tpope](https://github.com/tpope)'s [cobol.zip](http://www.vim.org/scripts/script.php?script_id=1655)
