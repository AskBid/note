```
:h rtp
```

inside nvim (explorer) shows which nvim folders nvim looks into

`%` creates new file

init.lua is the first place where nvim will look .config/nvim/nvim.lua

`d` to create directory

`.confi/nvim/lua` all queriable dirs in lua.

`config/nvim/lua/mare/init.lua`

`Ex` in Normal mode to get to explore in vim.

in `nvim.lua` (basically is index.html) add `require("mare")` to load that user folder with lua.

 like `Ex` so we create a `../mare/remap.lua` 

`:Mason` look for language, press `i` to accept it.

`:Telescope help_tags`
`:Telescope keymaps`


To find a word press `/` then type word, press `Enter` for then press `n` or `N` to go to next prev word found.

shift v `V` sets you in V-LINE mode where pasting and copying actually works on lines.

normal mode `d4k` deletes 4 rows above `d5w` deletes five words

`V` selects the whole row, then you can i.e. `y` move to new line select it `V` and substiutite with yanked by `p`