# plotterbrains

![first 6 renders](https://github.com/Frungy/plotterbrains/blob/main/rendersfirst6small.jpg)

 -------- Plotter Brains? --------
 
plotterbrains.hs is the brains of a 3D graphing calculator that can output 3D graphs to Minecraft.


 -------- What? --------
 
Think of it like a graphing calculator, like a TI-83, but with some differences.  A typical graphing calculator plots in black and white two-dimensional pixels.  This one plots in 3D, in color (with textures), and with enough computing power that you can fly around wasd-style in the graphs.

Or think of it like the 3D graphing functionality of Matlab or Maple or Mathematica, but all voxel-based.

Technically, the way plotterbrains.hs works is that Minecraft is used as the renderer and plotterbrains.hs is the brains of the plotter, hence the name.  By that, I mean that plotterbrains computes 3D math things and figures out where all the cubes have to go and what color they have to be, then doesn't bother rendering that but hands it off to Minecraft at that point, to put it briefly.  Actually, plotterbrains.hs is set up to output things that are taken by MCEdit as inputs, then MCEdit puts the graphs into Minecraft worlds, and then Minecraft renders them wasd-style.


 -------- What's Next in Dev for this code? --------
 
Hello.  I don't know anything about software things, and I'm entirely uninterested in learning that stuff.  For example, I want to use one of those programs that can put the blocks into the Minecraft world within Minecraft while it's running, or even just have a program that can put blocks into worlds automatically, but I've been stuck with running each plot through MCEdit in GUI mode (and staring at many loading bars), because I don't know anything about software things, and I've failed every time I've tried to learn any of it.  For example, I found many things for both of those methods, and tried learning several of them, and found the instructions incomprehensible every time and wasted many dozens of hours trying to understand things without knowing how to look up what I don't know.  Get the idea??  I don't know anything about software things, okay?  My haskell programming is also bad.

I'm looking for people who are willing to help in ways that don't involve me learning that goddamn arcana (I mean everything aside from Haskell - I'm cool with writing bad Haskell).  I'm willing to work collaboratively, write specifications, make mockups (and I can do those really well, given my actual skllset), et cetera.  Questions include: those things that already exist and would speed up the workflow of producing graph worlds, how the hell do you install and run them?  And so on.  I think there are ways to make all this work better, even given how stupid I am.  There are also additional plotting abilities that could be added to plotterbrains.  I also don't know how to do anything with moving blocks, and there are some mind-croggling plotting abilities that could be added to plotterbrains if anyone could explain how to do any of that to an actual human.


 -------- Full Instructions --------

This full instructions section contains two subsections.

 () Introduction to the full instructions ()
 
Once again, the three programs, in order, are: plotterbrains, then MCEdit, then Minecraft.  Here's the brief rundown of how to get them to talk to each other.  Make a new world in Minecraft creative mode, then exit Minecraft.  Then open MCEdit and load up that world you just created.  Get plotterbrains to give you a plot in the form of a .py file.  Copy that .py file into MCEdit's filters folder.  Select some space where the graph goes in MCEdit and run that filter - that puts the blocks in the world.  Then save, then close MCEdit.  Then open Minecraft and go back into that world and the graph will be in it.  Then you can fly around it.

 () The rest of the full instructions ()
 
plotterbrains.hs has several goers that all start with "go".  For example, if you load plotterbrains.hs in GHCi and then say "go_w01", then it will output the .py files for graph world 1, which is a set of 6 thin-slice graphs.  I don't know if compiling makes it run faster, but, for example, you can change the line "main" to say "main = go_w08" and then compile and you'll get an .exe that, when run, outputs the .py files for graph world 8 (and that one has a lot of blocks).  plotterbrains has a number of plotting abilities (e.g. plot a 3D scalar field, plot a 2D vector field, etc.) and a number of graph worlds which call on those plotting abilities to make the graphs like in the pictures and videos.

This works with MCEdit 1.5.6.0, but I think it doesn't work with MCEdit version 2 (but I don't remember exactly).  To get the graph into the world, you'll need to have that part of the world generated first.  For example, if the plot is a small graph near (0,100,0) and your new Minecraft world had you spawn at (300,100,300), then the part of that world near that graph won't be generated when the world is generated.  To fix that, you can fly to (0,100,0) within Minecraft and it will generate some world there.  When plotting a big graph, you might have to fly around for a while to make sure there's enough world generated around where the graph goes.  Once the world and the filters are ready, open that world in MCEdit and select some space around where the graph goes.  You can use ctrl+A to select a really big zone, but that will make the filter run slower.  Once you've selected where the graph goes, click on filters, and if you've put the .py in MCEdit's filters folder, then you should see it on the list of filters that you can run.  Run the filters (plotterbrains is set up to break apart big graphs across multiple filters) with that space selected and then the blocks should appear.  The rendering power of MCEdit is limited, so if you have a big graph, you'll have to fly around in MCEdit and maybe only part of the graph will show up at a time, but the blocks are all there.  Save, exit MCEdit, and the Minecraft world is ready.  Load up that world in Minecraft and the graph should be there.  Then you can fly around it.

Because I don't know how to do it the better way, or what the better way is, or how to learn what the better way is.


![second 6 renders](https://github.com/Frungy/plotterbrains/blob/main/renderssecond6small.jpg)
