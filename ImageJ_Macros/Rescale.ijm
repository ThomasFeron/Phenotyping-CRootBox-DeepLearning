dir = File.directory
title = getTitle; 
run("Set Scale...", "distance=2099 known=17.78 pixel=1 unit=cm");
saveAs("tiff",dir+title);