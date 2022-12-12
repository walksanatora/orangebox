# OrangeBox
(we went from advanced to command)
A Virtual machine for ComputerCraft<br>
this is basically a maintained fork of [MCJack123/yellowbox.lua](https://gist.github.com/MCJack123/e634347fe7a3025d19d9f7fcf7e01c24)<br>
<br>
since it is EOL for everyone outside of his propietary closed source OS (phoenix, which is ironic because it is called the linux of the CC comunity)<br>
<br>
Also adds support for disk compression via [LibDeflate](https://github.com/MCJack123/CC-Archive/LibDeflate.lua)<br>
(if it it is `require`able you unlock `yellowbox:setCompression(bool)`)<br>

# imgtool
this repo also contains `imgtool` which is a small archive tool that generates gzipped vfs files (which can be used with orangebox if it has LibDeflate)

it also has some usefull settings which can be used to set defaults ex:
`imgtool.overwite` - bool, determines whether or not the tool should by default overwrite the existing image set as the output file
`imgtool.dir_action` - string, a single letter either M/O/C (Merge,Overwrite,Cancel) determining what to do when extracting a image and the output directory allready exists