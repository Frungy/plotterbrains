# plotterbrains
plotterbrains is the brains of a 3D graphing calculator that can output 3D graphs to Minecraft.

![first 6 renders](https://github.com/Frungy/plotterbrains/blob/main/rendersfirst6small.jpg)

This is the version of plotterbrains by the guy who has no idea how to format things to be github things.

How to use this code:
The main idea with this program is that it's a Haskell program that outputs .py files when you run it, then if you run MCEdit (the Minecraft world editor, works with version 1.5.6.0, I think it doesn't work with version 2), and open a Minecraft world in MCEdit, and then run those .py files as filters, then it will add 3D graphs to that Minecraft world.  It's called plotterbrains because it's like the computey parts of a TI-83 graphing calculator, but it's 3D and in colors instead of the 2D black and white of your TI-83.  Think of Minecraft as being the screen of the TI-83 in this analogy.

Full instructions:
Get the .hs on your computer, open it with GHCi, type go_w01 to output the .py files for world 1, type go_w02 to output the .py files for world 2, et cetera, up to 5.  This is set up for 5 worlds at this point.  Also, if you do ghc --make plotterbrains.hs, then it will compile it up into an .exe.  I don't know if that makes it run faster or not.  Right now, main is hard-coded to go_w05, so if you compile it and run the .exe, it will output the .py files for world 5.  For worlds 1 through 4, it should only take a few seconds to run in interpreted mode anyways.  You can make a new Minecraft world in Minecraft or in MCEdit (I think).  What I do is make a new world in Minecraft, give it some name and start it in creative mode.  Then use f3 to bring up the on-screen information and fly to near x = 0, z = 0, and build a pillar there if you want.  Then close Minecraft.  Get MCEdit (prefer version 1.5.6.0), and copy the .py files from plotterbrains output to the "stock-filters" folder in the folder where you have MCEdit.  Then start up MCEdit, use quick load and find your new Minecraft world, then use ctrl+a to select-all (I think maybe it only selects one chunk) and then click on the coffee pot icon, which is filters.  When you pick a filter and run it, it will add blocks to the Minecraft world.  Each of these worlds is on multiple filters.  For example, world 1 is made of 6 filters, so if you did go_w01 in the .hs, it gave you 6 .py files that you copied over to MCEdit's filters folder.  Now run each of those one after the other and it will plot all of world 1.  Then use ctrl-s to save the Minecraft world.  Then close the world in MCEdit (or exit MCEdit), start up Minecraft, and go back into that world.  Now there should be 3D graphs and you should be able to fly around them.

What next:
I have no idea how to format a thing for github.  I'm guessing that putting everything into one .hs file and then putting that onto github is not exactly the right way to set one of these things up for github-related actitivites.  I hope someone will tell me how to do that because I think that's the next thing to do, and I hope they don't charge too much money.

What next after that:
I have more plotting abilities programmed, but I'll copy those in after this project is formatted appropriately.  Here's pictures of more graphs that are in my offline version but not this online one yet.

![second 6 renders](https://github.com/Frungy/plotterbrains/blob/main/renderssecond6small.jpg)
