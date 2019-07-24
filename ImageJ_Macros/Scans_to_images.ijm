macro "SCANS TO IMAGES TO ANALYZE" {

  dir = File.directory
  run("Set Scale...", "distance=236.22 known=1 pixel=1 unit=cm");
  run("Size...", "width=2400 height=3000 depth=1 constrain average interpolation=Bilinear");
  original = getImageID; 
  title = getTitle; 
  height = getHeight; 
  width = getWidth; 
  halfheight = height / 2;
  halfwidth = width / 2;

// UP_LEFT
  
 selectImage(original); 
  makeRectangle(halfwidth/2, 0, halfwidth/2, halfheight); 
  run("Duplicate...", "title=up_left_"+title+" duplicate");
  ul_title_o="up_left_"+title;
  selectImage(ul_title_o);
  run("Split Channels");
	selectWindow(ul_title_o+" (green)");
	close();
	selectWindow(ul_title_o+" (red)");
	close();
  ul_title="up_left_"+title+" (blue)";
  call("ij.plugin.frame.ThresholdAdjuster.setMode", "B&W");
  setThreshold(0, 190);
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  run("adjustable watershed", "tolerance=4");
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  if(nResults > 0){
  selectWindow("Results");
  maxParticuleArea = 0;
  for(i = 0; i < nResults; i++){
  	Particules_Area = getResult("Area",i);
  	if(getResult("Area", i) > maxParticuleArea){
  		maxParticuleArea = getResult("Area", i);
  		j=i;		
  	}
  }
  roiManager("Select", j);
  run("Crop");
  ul_title_o_3="new_"+ul_title_o;
  rename(ul_title_o_3);
  newImage("mix", "8-bit white", 2099, 2099, 1);
  run("Add Image...", "image="+ul_title_o_3+" x=1050 y=350 opacity=100");
  run("Flatten");
  run("Set Scale...", "distance=2100 known=17.78 pixel=1 unit=cm");
  saveAs("tiff",dir+"/Image_imageJ/"+ul_title_o);
  close();
  close();
  close();
  close();
  close();
  }
    
// UP_RIGHT
  
 selectImage(original); 
  makeRectangle(halfwidth+1, 0, halfwidth/2, halfheight); 
  run("Duplicate...", "title=up_right_"+title+" duplicate");
  ur_title_o="up_right_"+title;
  selectImage(ur_title_o);
    run("Split Channels");
    selectWindow(ur_title_o+" (green)");
	close();
	selectWindow(ur_title_o+" (red)");
	close();
  ur_title="up_right_"+title+" (blue)";
  call("ij.plugin.frame.ThresholdAdjuster.setMode", "B&W");
  setThreshold(0, 190);
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  run("adjustable watershed", "tolerance=4");
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  if(nResults > 0){
  selectWindow("Results");
  maxParticuleArea = 0;
  for(i = 0; i < nResults; i++){
  	Particules_Area = getResult("Area",i);
  	if(getResult("Area", i) > maxParticuleArea){
  		maxParticuleArea = getResult("Area", i);
  		j=i;		
  	}
  }
  
  roiManager("Select", j);
  run("Crop");
  ur_title_o_3="new_"+ur_title_o;
  rename(ur_title_o_3);
  newImage("mix", "8-bit white", 2099, 2099, 1);
  run("Add Image...", "image="+ur_title_o_3+" x=1050 y=350 opacity=100");
  run("Flatten");
  run("Set Scale...", "distance=2100 known=17.78 pixel=1 unit=cm");
  saveAs("tiff",dir+"/Image_imageJ/"+ur_title_o);
  close();
  close();
  close();
  close();
  close();
  }


// DOWN_LEFT  
 selectImage(original); 
  makeRectangle(halfwidth/2, halfheight, halfwidth/2, height); 
  run("Duplicate...", "title=down_left_"+title+" duplicate");
  dl_title_o="down_left_"+title;
  selectImage(dl_title_o);
  run("Split Channels");
	selectWindow(dl_title_o+" (green)");
	close();
	selectWindow(dl_title_o+" (red)");
	close();
  dl_title="down_left_"+title+" (blue)";
  call("ij.plugin.frame.ThresholdAdjuster.setMode", "B&W");
  setThreshold(0, 190);
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  run("adjustable watershed", "tolerance=4");
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  selectWindow(dl_title);
	close();
  selectWindow("Mask of "+dl_title );
	close();
  selectWindow("Mask of Mask of "+dl_title);
  rename(dl_title_o);
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  if(nResults > 0){
  selectWindow("Results");
  maxParticuleArea = 0;
  for(i = 0; i < nResults; i++){
  	Particules_Area = getResult("Area",i);
  	if(getResult("Area", i) > maxParticuleArea){
  		maxParticuleArea = getResult("Area", i);
  		j=i;		
  	}
  }
  
  roiManager("Select", j);
  run("Crop");
  dl_title_o_3="new_"+dl_title_o;
  rename(dl_title_o_3);
  newImage("mix", "8-bit white", 2099, 2099, 1);
  run("Add Image...", "image="+dl_title_o_3+" x=1050 y=350 opacity=100");
  run("Flatten");
  run("Set Scale...", "distance=2100 known=17.78 pixel=1 unit=cm");
  saveAs("tiff",dir+"/Image_imageJ/"+dl_title_o);
  close();
  close();
   
  selectWindow(dl_title_o_3);
	close();

  }	
  selectWindow(dl_title_o);
	close();

	


// DOWN_RIGHT  
  
selectImage(original); 
  makeRectangle(halfwidth+1, halfheight+1, halfwidth/2, height); 
  run("Duplicate...", "title=down_right_"+title+" duplicate");
  dr_title_o="down_right_"+title;
  selectImage(dr_title_o);
  run("Split Channels");
	selectWindow(dr_title_o+" (green)");
	close();
	selectWindow(dr_title_o+" (red)");
	close();
  dr_title="down_right_"+title+" (blue)";
  call("ij.plugin.frame.ThresholdAdjuster.setMode", "B&W");
  setThreshold(0, 190);
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  run("adjustable watershed", "tolerance=4");
  run("Analyze Particles...", "size=0.01-Infinity circularity=0.00-0.10 show=Masks display exclude clear add");
  
  if(nResults > 0){
  	selectWindow("Results");
  maxParticuleArea = 0;
  for(i = 0; i < nResults; i++){
  	Particules_Area = getResult("Area",i);
  	if(getResult("Area", i) > maxParticuleArea){
  		maxParticuleArea = getResult("Area", i);
  		j=i;		
  	}
  }
  
  roiManager("Select", j);
  run("Crop");
  dr_title_o_3="new_"+dr_title_o;
  rename(dr_title_o_3);
  newImage("mix", "8-bit white", 2099, 2099, 1);
  run("Add Image...", "image="+dr_title_o_3+" x=1050 y=350 opacity=100");
  run("Flatten");
  run("Set Scale...", "distance=2100 known=17.78 pixel=1 unit=cm");
  saveAs("tiff",dir+"/Image_imageJ/"+dr_title_o);
  close();
  close();
  close();
  close();
  close();

  }
// CLOSE WINDOWS  

if (isOpen(original)) { 
       selectImage(original); 
       run("Close"); 
   } 

if (isOpen("Results")) { 
       selectWindow("Results"); 
       run("Close"); 
   } 

if (isOpen("ROI Manager")) { 
       selectWindow("ROI Manager"); 
       run("Close"); 
   } 
   
while (nImages>0) { 
          selectImage(nImages); 
          close();
	}       
    
  
} 

