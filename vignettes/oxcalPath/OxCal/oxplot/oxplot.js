var canvas,plotInfo,plotOptions,plotData,editOptions,multiPlots,memInfo,tables,fileOptions;
var plotCalc,plotMultiCalc,plotColor,memCalc;
var editSpec,viewSpec,formatSpec,canvasSpec,multiSpec,dataSpec,fileSpec,colorSpec;
var autofill=false;
var plotPg="";
var dataCount=0;
var multiPos=0,xorg,yorg,worg,horg,yoffs;
var ticker=false;
var fileinclude=false;
var markers=new Array("","circle","square","diamond","triangle","cross","x","label","histogram","rectangle","image");
var colors=new Array("rgb(255,0,0)","rgb(0,255,0)","rgb(0,0,255)","rgb(204,204,0)", "rgb(204,0,204)","rgb(0,204,204)","rgb(127,127,127)");
   canvas=new Object;
   canvas.scale=1.0; // zoom scaling
   canvas.scaleFont=0.8; // font scaling
   canvas.scaleLine=1.3; // line scaling
   canvas.pxPerCm=35; // px per cm
   canvas.frameWidth=18.0; // cm
   canvas.frameHeight=13.5; // cm
   
   fileOptions=new Object;
   fileOptions.canvas=true; // covers canvas
   fileOptions.view=true; //covers plotInfo and multiPlots
   fileOptions.format=true; // covers plotOptions
   fileOptions.data=true;
   
   plotInfo=new Object;
   plotInfo.minx=0;
   plotInfo.maxx=100;
   plotInfo.autox=0;
   plotInfo.miny=0;
   plotInfo.maxy=100;
   plotInfo.autoy=0;
   plotInfo.x_calc="100*SiO2/total";
   plotInfo.y_calc="100*(K2O+Na2O)/total";
   plotInfo.dx_calc="";
   plotInfo.dy_calc="";
   plotInfo.xlabel="SiO2 (wt%)";
   plotInfo.ylabel="K2O + Na2O (wt%)";
   plotInfo.title="TAS";
   plotInfo.category="Majors";
   plotInfo.table="volcMajors";
   plotInfo.keyTitle="Key";
   plotInfo.pcaVariables="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O";
   
   plotColor=new Object;
   plotColor.showKey=false;
   plotColor.z_calc="";
   plotColor.dz_calc="";
   plotColor.zlabel="";
   plotColor.minz=0;
   plotColor.maxz=100;
   plotColor.autoz=0;
   plotColor.min_col="rgb(0,0,255)";
   plotColor.max_col="rgb(255,0,0)";
   
   plotData=new Array;
   
   plotOptions=new Object;
   plotOptions.background="rgb(255,255,255)";
   plotOptions.plotBackground="rgba(255,255,255,0)";
   plotOptions.mapForeground="#cccccc";
   plotOptions.multiPlot=false; 
   plotOptions.plotPosX=3.0; // cm
   plotOptions.plotPosY=1.5;  // cm
   plotOptions.plotWidth=11.5; // cm
   plotOptions.plotHeight=11.5; // cm
   plotOptions.showKey=1;
   plotOptions.showGrid=false;
   plotOptions.BandW=false;
   plotOptions.contours="95%";
   
   editOptions=new Object;
   editOptions.window=window.opener;
   if(window.parent && (window.parent!=window))
   {
    editOptions.window=window.parent;
    if(window.parent.opener && (window.parent.opener!=window.parent) && (!window.parent.app))
    {
     editOptions.window=window.parent.opener;
    };
   };
   editOptions.table=0;
   editOptions.division="";
   filename=false;
   
   multiPlots=new Object;
   memInfo=new Object;
   
   plotCalc=new Array;
var plotCalcMulti=new Array;   
   
   
 var s,ss;
tables=new Array;
var divisions=new Array;
var divisionsDisplay=new Array;
editSpec=new itemSpec("data","Import","Object");
editSpec.noheader=true;
editSpec.edit=true;
s=editSpec.appendChild("title","Title","Text");
s=editSpec.appendChild("text","Text","TextArea");
s.changer="changeEditOptions()";
s=editSpec.appendChild("table","Table","Number");
s.options=tables;
s.changer="changeEditOptions()";
s=editSpec.appendChild("division","Split using","Text");
s.options=divisions;
s.optionsDisplay=divisionsDisplay;
s=editSpec.appendChild("import","Import","Button");
s.readonly=true;
s.action="hideArea('editImport');suckData()";

fileSpec=new itemSpec("file","Include","Object");
fileSpec.edit=true;
s=fileSpec.appendChild("data","Data","Boolean");
s=fileSpec.appendChild("canvas","File options","Boolean");
s=fileSpec.appendChild("view","View details","Boolean");
s=fileSpec.appendChild("format","Format","Boolean");

viewSpec=new itemSpec("data","View","Object");
viewSpec.edit=true;
s=viewSpec.appendChild("title","Title","Text");
s=viewSpec.appendChild("xlabel","X axis label","Text");
s=viewSpec.appendChild("x_calc","X value","Text");
s=viewSpec.appendChild("dx_calc","X error","Text");
s=viewSpec.appendChild("minx","Min X","Number");
s=viewSpec.appendChild("maxx","Max X","Number");
s=viewSpec.appendChild("autox","Auto X","Number");
s.options=["off","ascending","descending"];
s=viewSpec.appendChild("nonlinx","Non linear X","Text");
s.options=["","log"];
s=viewSpec.appendChild("ylabel","Y axis label","Text");
s=viewSpec.appendChild("y_calc","Y value","Text");
s=viewSpec.appendChild("dy_calc","Y error","Text");
s=viewSpec.appendChild("miny","Min Y","Number");
s=viewSpec.appendChild("maxy","Max Y","Number");
s=viewSpec.appendChild("autoy","Auto Y","Number");
s.options=["off","ascending","descending"];
s=viewSpec.appendChild("nonliny","Non linear Y","Text");
s.options=["","log","mercator"];
s=viewSpec.appendChild("mapPlot","Map underlay","Boolean");
s=viewSpec.appendChild("weight","Weight","Text");
s=viewSpec.appendChild("keyTitle","Key title","Text");
s=viewSpec.appendChild("find","Find in range","Button");
s.action="hideArea('viewPlot');findInRange(this)";
s.readonly=true;
s=viewSpec.appendChild("ok","OK","Button");
s.action="hideArea('viewPlot');setValues(this.parent)";
s.readonly=true;
s=viewSpec.appendChild("cancel","Cancel","Button");
s.action="hideArea('viewPlot')";
s.readonly=true;

colorSpec=new itemSpec("data","Color scale","Object");
colorSpec.edit=true;
s=colorSpec.appendChild("showKey","Show scale","Boolean");
s=colorSpec.appendChild("zlabel","Label","Text");
s=colorSpec.appendChild("z_calc","Z value","Text");
s=colorSpec.appendChild("dz_calc","Z error","Text");
s=colorSpec.appendChild("minz","Min Z","Number");
s=colorSpec.appendChild("maxz","Max Z","Number");
s=colorSpec.appendChild("autoz","Auto Z","Number");
s.options=["off","ascending","descending"];
s=colorSpec.appendChild("min_col","Min color","Color");
s=colorSpec.appendChild("max_col","Max color","Color");
s=colorSpec.appendChild("ok","OK","Button");
s.action="hideArea('viewColor');setValues(this.parent)";
s.readonly=true;
s=colorSpec.appendChild("cancel","Cancel","Button");
s.action="hideArea('viewColor')";
s.readonly=true;

formatSpec=new itemSpec("data","Format","Object");
formatSpec.edit=true;
s=formatSpec.appendChild("multiPlot","Multi plot","Boolean");
s=formatSpec.appendChild("background","Background","Color");
s=formatSpec.appendChild("plotBackground","Plot background","Color");
s=formatSpec.appendChild("mapPlot","Map underlay","Text");
s.options=new Array("svg","roadmap","satellite","hybrid","terrain");
s=formatSpec.appendChild("mapForeground","SVG map foreground","Color");
s=formatSpec.appendChild("plotPosX","Left margin (cm)","Number");
s=formatSpec.appendChild("plotPosY","Bottom margin (cm)","Number");
s=formatSpec.appendChild("plotWidth","Width (cm)","Number");
s=formatSpec.appendChild("plotHeight","Height (cm)","Number");
s=formatSpec.appendChild("showKey","Show key","Number");
s.options=new Array("No key","Key with plot","Key only");
s=formatSpec.appendChild("showGrid","Show grid","Boolean");
s=formatSpec.appendChild("contours","Contours","Text");
s.options=new Array("95%","5","10","20");
s=formatSpec.appendChild("BandW","BandW","Boolean");
s=formatSpec.appendChild("ok","OK","Button");
s.action="setValues(this.parent)";
s.readonly=true;

canvasSpec=new itemSpec("data","View","Object");
canvasSpec.edit=true;
s=canvasSpec.appendChild("scale","Scale","Number");
s=canvasSpec.appendChild("scaleFont","Font scale","Number");
s=canvasSpec.appendChild("scaleLine","Line spacing","Number");
s=canvasSpec.appendChild("frameWidth","Canvas Width (cm)","Number");
s.readonly=true;
s=canvasSpec.appendChild("frameHeight","Canvas Height (cm)","Number");
s.readonly=true;
s=canvasSpec.appendChild("pxPerCm","Pixels/cm","Number");
s=canvasSpec.appendChild("ok","OK","Button");
s.action="document.getElementById('canvasOptions').style.display='none';setValues(this.parent)";
s.readonly=true;

dataSpec=new itemSpec("data","Data","Array");
dataSpec.edit=true;
dataSpec.database=true;
s=dataSpec.appendChild("name","Name","Text");
s=dataSpec.appendChild("line","Line","Text");
s.options=new Array("","solid","dashed","dotted");
s=dataSpec.appendChild("lineColor","Line color","Color");
s.popup=true;
s=dataSpec.appendChild("marker","Marker","Text");
s.options=markers;
s=dataSpec.appendChild("markerColor","Marker color","Color");
s.popup=true;
s=dataSpec.appendChild("markerFill","Marker fill","Color");
s.popup=true;
s=dataSpec.appendChild("contour","Contour","Text");
s.options=new Array("","Ellipse","KDE");
s=dataSpec.appendChild("singleOnly","Single plots only","Boolean");
s.popup=true;
s=dataSpec.appendChild("dataEdit","Data","Button");
s.readonly=true;
s.action="startEditData(this)";
s=dataSpec.appendChild("id","ID","Text");
s.popup=true;
s=dataSpec.appendChild("include_label","Include labels","Boolean");
s.popup=true;
s=dataSpec.appendChild("include_id","Include IDs","Boolean");
s.popup=true;
s=dataSpec.appendChild("include_url","Include URLs","Boolean");
s.popup=true;

multiSpec=new itemSpec("data","Multiplots","Object");
s=multiSpec.appendChild("plots","Plots","Array");
s.database=true;
ss=s.appendChild("plot","Plot","Button");
ss.readonly=true;
ss.action="choosePlot(this)";
ss=s.appendChild("title","Title","Text");
ss=s.appendChild("xlabel","X axis label","Text");
ss=s.appendChild("x_calc","X value","Text");
ss.popup=true;
ss=s.appendChild("dx_calc","X error","Text");
ss.popup=true;
ss=s.appendChild("minx","Min X","Number");
ss.popup=true;
ss=s.appendChild("maxx","Max X","Number");
ss.popup=true;
ss=s.appendChild("autox","Auto X","Number");
ss.options=["off","ascending","descending"];
ss.popup=true;
ss=s.appendChild("nonlinx","Non linear X","Text");
ss.options=["","log"];
ss.popup=true;
ss=s.appendChild("ylabel","Y axis label","Text");
ss=s.appendChild("y_calc","Y value","Text");
ss.popup=true;
ss=s.appendChild("dy_calc","Y error","Text");
ss.popup=true;
ss=s.appendChild("miny","Min Y","Number");
ss.popup=true;
ss=s.appendChild("maxy","Max Y","Number");
ss.popup=true;
ss=s.appendChild("autoy","Auto Y","Number");
ss.options=["off","ascending","descending"];
ss.popup=true;
ss=s.appendChild("nonliny","Non linear Y","Text");
ss.options=["","log","mercator"];
ss.popup=true;
ss=s.appendChild("mapPlot","Map underlay","Boolean");
ss.popup=true;
ss=s.appendChild("weight","Weight","Text");
ss.popup=true;
ss=s.appendChild("row","Row","Number");
ss=s.appendChild("column","Column","Number");
s=multiSpec.appendChild("columns","Columns","Number");
s.options=new Array("Single plot","1 column","2 column","3 column","4 column","5 column","6 column","7 column","8 column");
s=multiSpec.appendChild("tiedXAxes","Tied x-axes","Number");
s.options=new Array("Not tied","Tied","Without label","Without numbers","Without axis");
s=multiSpec.appendChild("tiedYAxes","Tied y-axes","Number");
s.options=new Array("Not tied","Tied","Without label","Without numbers","Without axis");
s=multiSpec.appendChild("interXPlot","Horizontal spacing (cm)","Number");
s=multiSpec.appendChild("interYPlot","Vertical spacing (cm)","Number");
s=multiSpec.appendChild("showKey","Show key","Boolean");
s=multiSpec.appendChild("keyRow","Key row","Number");
s=multiSpec.appendChild("keyColumn","Key column","Number");
s=multiSpec.appendChild("arrange","Auto arrange","Button");
s.action="arrangePlots()";
s.readonly=true;
s=multiSpec.appendChild("find","Find in range","Button");
s.action="hideArea('viewMultiplot');findInRange(this)";
s.readonly=true;
s=multiSpec.appendChild("ok","OK","Button");
s.action="hideArea('viewMultiplot');setValues(this.parent)";
s.readonly=true;
s=multiSpec.appendChild("cancel","Cancel","Button");
s.action="hideArea('viewMultiplot')";
s.readonly=true;

 function hideArea(area)
 {
  document.getElementById(area).style.display='none';
 };
 function showArea(area)
 {
  document.getElementById(area).style.display='block';
 };
 function parseColor(col)
 {
  var rtn=new Array,i;
  if(col.indexOf("#")==0)
  {
   rtn[0]=Math.round(parseInt(col.substring(1,3),16));
   rtn[1]=Math.round(parseInt(col.substring(3,5),16));
   rtn[2]=Math.round(parseInt(col.substring(5,7),16));
   return rtn;
  }
  else
  {
   rtn=col.replace("a(","(").replace("rgb(","").replace(")","").split(",");
   for(i=0;i<rtn.length;i++)
   {
    if(i>2)
    {
     rtn[i]=parseFloat(rtn[i]);
    }
    else
    {
     rtn[i]=parseInt(rtn[i]);
    };
   };
  };
  return rtn;
 };
 function saveDefaults()
 {
  var str;
  str=displayItem(canvas,"canvas");
  str+=displayItem(plotOptions,"plotOptions");
  str+=displayItem(plotInfo,"plotInfo");
  setCookie("xyPlotSettings",str);
 };
 function restoreDefaults()
 {
  var str;
  str=getCookie("xyPlotSettings");
  if(str){eval(str);};
  plotOptions.plotPosX=3.0; // cm
  plotOptions.plotPosY=1.5;  // cm
  plotOptions.plotWidth=11.5; // cm
  plotOptions.plotHeight=11.5; // cm
 };
 function resetDefaults()
 {
  setCookie("xyPlotSettings","");
  window.location.reload();
 };
 function setFilename(fname)
 {
  filename=fname;
  document.title=filename;
 };
 function getFilename()
 {
  return filename;
 };
 function getFileContent()
 {
  if(filename.indexOf(".plot")!=-1)
  {
   fileSpec.refillContainer();
   filecontent="";
   if(fileOptions.canvas)
   {
    filecontent+=displayItem(canvas,"canvas");
   };
   if(fileOptions.view)
   {
    filecontent+=displayItem(plotInfo,"plotInfo");
    filecontent+=displayItem(plotColor,"plotColor");
    filecontent+=displayItem(multiPlots,"multiPlots");
   };
   if(fileOptions.format)
   {
    filecontent+=displayItem(plotOptions,"plotOptions");
   };
   if(fileOptions.data)
   {
    filecontent+=displayItem(plotData,"plotData");
   };
  };
  return filecontent;
 };
 function includeFile(f)
 {
  fileinclude=f;
  document.getElementById('fileFrame').src="oxplot_load.html";
 };
 function darkBackground()
 {
  var rgb;
  rgb=parseColor(plotOptions.background);
  return (rgb[0]+rgb[1]+rgb[2]) < 384;
 };
 function cleanDatabaseArray(spc,ar)
 {
  var i,j;
  spc.emptyContainer();
  for(i=0;i<ar.length;i++)
  {
   ar[i].changed=false;
   while((i<ar.length)&&(typeof(ar[i].deleted)!="undefined")&&(ar[i].deleted))
   {
    for(j=i+1;j<ar.length;j++)
    {
     ar[j-1]=ar[j];
    };
    ar.length--;
   };
  };
  spc.fillContainer();  
 };
 function keyItems()
 {
  var i,items=0;
  for(i=0;i<plotData.length;i++)
  {
   if(plotData[i].selected && plotData[i].name)
   {
    items++;
   };
  };
  return items;
 };
 function keyWidth()
 {
  var i,width=0;
  for(i=0;i<plotData.length;i++)
  {
   if(plotData[i].selected && plotData[i].name)
   {
    if(plotData[i].name.length > width)
    {
     width=plotData[i].name.length;
    };
   };
  };
  return width;
 };
 function setCanvasSize()
 {
  var i,scltst;
  yoffs=0;
  canvas.frameWidth=plotOptions.plotPosX+plotOptions.plotWidth+0.5;
  canvas.frameHeight=1.0+plotOptions.plotPosY+plotOptions.plotHeight;
  plotOptions.plotPosYOffs=0.5;
  if(plotOptions.showKey && keyItems() && !plotOptions.multiPlot)
  {
   plotOptions.key=true;
   plotOptions.keyWidth=3+keyWidth()*canvas.scaleFont*0.3;
   plotOptions.keyHeight=0.4*(keyItems()+2)*canvas.scaleLine*canvas.scaleFont;
   if(plotOptions.keyHeight > plotOptions.plotHeight)
   {
    plotOptions.plotPosYOffs=plotOptions.keyHeight-plotOptions.plotHeight;
    canvas.frameHeight+=plotOptions.plotPosYOffs;
   };
   switch(plotOptions.showKey)
   {
   case 1:
    plotOptions.keyPosX=plotOptions.plotPosX+plotOptions.plotWidth+0.5;
    canvas.frameWidth+=plotOptions.keyWidth+0.5;
    break;
   case 2:
    plotOptions.keyPosX=0.5;
    canvas.frameWidth=plotOptions.keyWidth;
    break;
   };
   plotOptions.keyPosY=plotOptions.plotPosY+plotOptions.plotHeight-plotOptions.keyHeight;
   plotOptions.keyCount=keyItems();
  }
  else
  {
   plotOptions.key=false;
  };
  if(plotOptions.multiPlot)
  {
   if(multiPlots.columns==0)
   {
    plotOptions.cellWidth=0;
    plotOptions.cellHeight=0;
   }
   else
   {
    plotOptions.cellWidth=canvas.frameWidth;
    if(multiPlots.tiedYAxes)
    {
     switch(multiPlots.tiedYAxes)
     {
     case 2: plotOptions.cellWidth-=(plotOptions.plotPosX/2); break;
     case 3: case 4: plotOptions.cellWidth-=plotOptions.plotPosX; break;
     };
    };
    if(multiPlots.interXPlot){plotOptions.cellWidth+=multiPlots.interXPlot;};
    plotOptions.cellHeight=canvas.frameHeight;
    if(multiPlots.tiedXAxes)
    {
     switch(multiPlots.tiedXAxes)
     {
     case 2: plotOptions.cellHeight-=(plotOptions.plotPosY/2); break;
     case 3: case 4: plotOptions.cellHeight-=plotOptions.plotPosY; break;
     };
    };
    if(multiPlots.interYPlot){plotOptions.cellHeight+=multiPlots.interYPlot;};
   };
   plotOptions.maxrow=0;
   for(i=0;i<multiPlots.plots.length;i++)
   {
    if(multiPlots.plots[i].selected && (multiPlots.plots[i].row>plotOptions.maxrow))
    {
     plotOptions.maxrow=multiPlots.plots[i].row;
    };
   };
   if(multiPlots.columns>1)
   {
    canvas.frameWidth+=(multiPlots.columns-1)*plotOptions.cellWidth;
   };
   plotOptions.columns=multiPlots.columns;
   if(plotOptions.maxrow>0)
   {
    canvas.frameHeight+=(plotOptions.maxrow)*plotOptions.cellHeight;
   };
   if(multiPlots.showKey)
   {
    plotOptions.keyWidth=3+keyWidth()*canvas.scaleFont*0.3;
    plotOptions.keyHeight=0.4*(keyItems()+2)*canvas.scaleLine*canvas.scaleFont;
    plotOptions.keyPosX=0.5;
    plotOptions.keyPosY=plotOptions.plotPosY+plotOptions.plotHeight-plotOptions.keyHeight;
    plotOptions.keyCount=keyItems();
    tst=multiPlots.keyRow+(plotOptions.keyHeight+1)/plotOptions.cellHeight;
    if(tst>plotOptions.maxrow+1)
    {
     canvas.frameHeight+=(tst-1-plotOptions.maxrow)*plotOptions.cellHeight;
     yoffs=(tst-1-plotOptions.maxrow)*plotOptions.cellHeight;
    };
    tst=multiPlots.keyColumn+(plotOptions.keyWidth+1)/plotOptions.cellWidth;
    if(tst>multiPlots.columns)
    {
     canvas.frameWidth+=(tst-multiPlots.columns)*plotOptions.cellWidth;
    };
   };
   canvas.frameWidth+=plotOptions.plotPosX;
   canvas.frameHeight+=plotOptions.plotPosY;
  };
  if(plotColor.showKey)
  {
   plotOptions.colPosX=canvas.frameWidth+plotOptions.plotPosX;
   plotOptions.colPosY= plotOptions.plotPosY;
   if(plotOptions.multiPlot)
   {
    plotOptions.colPosY+=plotOptions.maxrow*plotOptions.cellHeight;
   };
   canvas.frameWidth+=plotOptions.plotPosX+2;
  };
  canvasSpec.refillContainer(true);
 };
 function setValues(spec,leaveValues)
 {
  var pg;
  switch(spec)
  {
  case viewSpec:
   plotOptions.multiPlot=false;
   if(leaveValues){formatSpec.object=plotOptions;};
   formatSpec.refillContainer(true);
   spec.refillContainer(leaveValues);
   break;
  case colorSpec:
   if(leaveValues)
   {
    colorSpec.object=plotColor;
    formatSpec.object=plotOptions;
   };
   formatSpec.refillContainer(true);
   spec.refillContainer(leaveValues);
   break;
  case dataSpec:
   if(!leaveValues)
   {
   	cleanDatabaseArray(dataSpec,plotData);
   }
   else
   {
    dataSpec.object=plotData;
    spec.refillContainer(leaveValues);
   };
   break;
  case multiSpec:
   plotOptions.multiPlot=true;
   multiSpec.object=multiPlots;
   formatSpec.refillContainer(true);
   multiSpec.edit=false;
   if(!leaveValues)
   {
   	cleanDatabaseArray(multiSpec,multiPlots.plots);
   }
   else
   {
    spec.refillContainer(leaveValues);
   };
   break;
  default:
   if(spec)
   {
    spec.refillContainer(leaveValues);
   }
   else
   {
    if(leaveValues)
    {
     canvasSpec.object=canvas;
     viewSpec.object=plotInfo;
     colorSpec.object=plotColor;
     multiSpec.object=multiPlots;
     formatSpec.object=plotOptions;
     dataSpec.object=plotData;
    };
    canvasSpec.refillContainer(leaveValues);
    viewSpec.refillContainer(leaveValues);
    colorSpec.refillContainer(leaveValues);
    multiSpec.refillContainer(leaveValues);
    formatSpec.refillContainer(leaveValues);
    dataSpec.refillContainer(leaveValues);
   };
   break;
  };
  if(multiPlots.plots.length==0)
  {
   plotOptions.multiPlot=false;
   formatSpec.refillContainer(true);
  };
  setCanvasSize();
  makeActive('main',-1);
  if(!window.frames[0] || !window.frames[0].mover || !window.frames[0].mover.active)
  {
   calcData(false);
  };
  if(plotOptions.background.indexOf("rgba")==-1)
  {
   document.getElementById("plotFrame").style.backgroundColor=plotOptions.background;
  }
  else
  {
   document.getElementById("plotFrame").style.backgroundColor="";
  };
  if(darkBackground())
  {
   if(plotInfo.mapPlot && (plotOptions.mapPlot=="svg"))
   {
    pg="xy_plot_map_rev_svg.html";
   }
   else
   {
    pg="xy_plot_rev_svg.html";
   };
  }
  else
  {
   if(plotInfo.mapPlot && (plotOptions.mapPlot=="svg"))
   {
    pg="xy_plot_map_svg.html";
   }
   else
   {
    pg="xy_plot_svg.html";
   };
  };
  if(plotPg!=pg)
  {
   document.getElementById("plotFrame").src=pg;
   plotPg=pg;
  }
  else
  {
   if(typeof(window.frames[0].createPlot)!='undefined')
   {
    window.frames[0].createPlot();
   }
   else
   {
    document.getElementById("plotFrame").src=pg;
   };
  };
  if(parent.onShowPlot){parent.onShowPlot();};
  saveDefaults();
 };
 function changeEditOptions(tryPlot)
 {
  var i,j,chld,hed,frst,lines;
  divisions[0]="";divisionsDisplay[0]="";tables[0]="[Text]";
  divisions[1]="selected";divisionsDisplay[1]="Selected";
  divisions.length=2;divisionsDisplay.length=2;tables.length=1;
  editSpec.emptyContainer();
  if(editOptions.window)
  {
   if(!editOptions.window.closed && editOptions.window.dbDataSpec)
   {
    for(i=0;i<editOptions.window.dbDataSpec.length;i++)
    {
     if(!editOptions.window.dbDataSpec[i]){continue;};
     tables[i]=editOptions.window.dbDataSpec[i].prompt;
     if(tables[i]==plotInfo.category && tryPlot)
     {
      editOptions.table=i;
      autofill=true;
     };
     if((autofill && (editOptions.table==i)))
     {
      for(j=0;j<editOptions.window.dbDataSpec[i].children.length;j++)
      {
       chld=editOptions.window.dbDataSpec[i].children[j];
       if((chld.type!="Number")&&(chld.type!="Date"))
       {
        divisions.push(chld.name);
        divisionsDisplay.push(chld.prompt);
       };
      };
     };
    };
    tables[i]="[Text]";
   };
  };
  if(tables[editOptions.table]=="[Text]")
  {
   editSpec.children[0].hidden=false;
   editSpec.children[1].hidden=false;
   lines=editOptions.text.split("\n");
   if(lines.length>1)
   {
    hed=lines[0].split("\t");
    frst=lines[1].split("\t");
    for(i=0;i<hed.length;i++)
    {
     if(isNaN(frst[i]))
     {
      divisions.push(hed[i]);
      divisionsDisplay.push(hed[i]);
     };
    };
   };
  }
  else
  {
   editSpec.children[0].hidden=true;
   editSpec.children[1].hidden=true;
  };
  editSpec.fillContainer();
 };
 function setDataTables(firstTime)
 {
  tables.length=0;
  autofill=false;
  changeEditOptions(true);
  if(!firstTime)
  {
   suckPlotInformation();
  };
  if(editOptions.window && editOptions.window.rawPlotData)
  {
   suckData(duplRemoteItem(editOptions.window,editOptions.window.rawPlotData));
  };
  if(autofill)
  {
   suckData();
  }
  else
  {
   if(firstTime && plotData.length)
   {
    setValues(dataSpec);
   };
  };
 };
 function xml2Str(xmlNode)
 {
  try {
    return (new XMLSerializer()).serializeToString(xmlNode);
  }
  catch (e) {
    try {
      // Internet Explorer.
      return xmlNode.xml;
    }
    catch (e)
    {
     alert('Xmlserializer not supported');
    };
  };
  return false;
 };
 function getSVGContent()
 {
   var cont;
   cont=xml2Str(window.frames[0].document.getElementById("top"));
   if(cont)
   {
    // get rid of duplicate header (Opera)
    cont=cont.replace('\<\?xml version="1.0"\?\>\<\?xml version="1.0" \?\>','\<\?xml version="1.0" \?\>');
    // add header if not there at all (Safari)
    if(cont.indexOf("\<\?xml")==-1)
    {
     cont="\<\?xml version=\"1.0\" encoding=\"UTF-8\"\?\>"+cont;
    };
    // get rid of any non-ascii characters
//    cont=cont.replace(/[^\u000A\u000D\u0020-\u007F]/g,"_");
    // remove problem parts of IE9 header
    if(cont.indexOf("xmlns:NS1=")!=-1)
    {
     cont=cont.replace(' xmlns:NS1=""','');
     cont=cont.replace(' NS1:xmlns:ev="http://www.w3.org/2001/xml-events"','');
     cont=cont.replace(' version="1.1"','');
    };
    // correct problems with Safari symbol links
    cont=cont.replace(/ xlink\=/g,' xmlns:xlink=');
    cont=cont.replace(/use href\=/g,'use xlink:href=');
    return cont;
   };
   return false;
 };
 function checkCanSave()
 {
  var i,ok=true;
  dataSpec.emptyContainer();
  for(i=0;i<plotData.length;i++)
  {
   if(plotData[i].include_url)
   {
    plotData[i].include_url=false;
    ok=false;
   };
  };
  dataSpec.fillContainer();
  formatSpec.emptyContainer();
  if(plotInfo.mapPlot)
  {
   if(plotOptions.mapPlot!='svg')
   {
    plotOptions.mapPlot='svg';
    ok=false;
   };
  };
  formatSpec.fillContainer();
  if(!ok){setValues();};
  if(!ok)
  {
   alert("Setting up for plot that can be saved...");
  };
  return ok;
 };
 function fileClick(op)
 {
  switch(op)
  {
  case "Open...":
   fileSpec.emptyContainer();
   fileSpec.fillContainer();
   fileDialog("Open","plot");
   showArea('fileOptionsArea');
   showArea('fileDialog');
   break;
  case "Save...":
   fileSpec.emptyContainer();
   fileSpec.fillContainer();
   filename=filename.split('.')[0]+".plot";
   saveFileAs("plot");
   showArea('fileOptionsArea');
   showArea('fileDialog');
   break;
  case "Save plot":
   if(!checkCanSave())
   {
    break;
   };
   filename=filename.split('.')[0]+".svg";
   filecontent=getSVGContent();
   if(filecontent)
   {
	saveFileAs("svg");
    hideArea('fileOptionsArea');
    showArea('fileDialog');
   }
   else
   {
	alert("Browser does not support function");
   };
   break;
  case "Export data":
   filename=filename.split('.')[0]+".csv";
   saveAllData();
   break;
  case "Print":
   window.frames[0].print();
   break;
  case "Options":
   showArea("canvasOptions");
   break;
  case "Reset":
   resetDefaults();
   break;
  };
 };
 function toolClick(op)
 {
  showArea("tool"+op);
 };
 function editClick(op)
 {
  showArea("edit"+op);
 };
 function viewClick(op)
 {
  switch(op)
  {
  case "Toggle markers":
   toggleMarkers();
   break;
  case "Toggle lines":
   toggleLines();
   break;
  case "Toggle ellipses":
   toggleEllipses();
   break;
  case "Toggle KDE":
   toggleKDEContours();
   break;
  case "Single plot":
   showArea("viewPlot");
   break;
  case "Color scale":
   showArea("viewColor");
   break;
  default:
   showArea("view"+op);
   break;
  };
 };
 function innerMenu(lev)
 {
  switch(lev)
  {
  case 0:
   clickmenu("file","Open...|Save...|Save plot|Export data|Print||Options|Reset",true);
   break;
  case 1:
   clickmenu("edit","Data|Import",true);
   break;
  case 2:
   clickmenu("view","Single plot|Multiplot|Color scale||Toggle markers|Toggle lines|Toggle ellipses|Toggle KDE",true);
   break;
  case 3:
   document.writeln("<div id='formatArea' class='toolbar' style='left:0px'><\/div>");
   break;
  case 4:
   clickmenu("tool","PCA|Ellipsoid matrix|KDE matrix",true);
   break;
  };
 };
 function setMPvalues(from_obj,to_obj,firstTrial,nolabel)
 {
  to_obj.title=from_obj.title;
  if(!nolabel)
  {
   to_obj.xlabel=from_obj.xlabel;
   to_obj.ylabel=from_obj.ylabel;
  };
  if(multiPlots.tiedXAxes && (!firstTrial))
  {
   from_obj.minx=to_obj.minx;
   from_obj.maxx=to_obj.maxx;
   to_obj.autox=0;
   from_obj.autox=to_obj.autox;
  }
  else
  {
   to_obj.minx=from_obj.minx;
   to_obj.maxx=from_obj.maxx;
   to_obj.autox=from_obj.autox;
  };
  to_obj.x_calc=from_obj.x_calc;
  to_obj.dx_calc=from_obj.dx_calc;
  if(multiPlots.tiedYAxes && (!firstTrial))
  {
   from_obj.miny=to_obj.miny;
   from_obj.maxy=to_obj.maxy;
   to_obj.autoy=0;
   from_obj.autoy=to_obj.autoy;
  }
  else
  {
   to_obj.miny=from_obj.miny;
   to_obj.maxy=from_obj.maxy;
   to_obj.autoy=from_obj.autoy;
  };
  to_obj.y_calc=from_obj.y_calc;
  to_obj.dy_calc=from_obj.dy_calc;
  to_obj.xtop=from_obj.xtop;
  to_obj.yright=from_obj.yright;
  to_obj.weight=from_obj.weight;
  to_obj.hideRectangle=from_obj.hideRectangle;
 };
 function choosePlot(spec)
 {
  hideArea('viewMultiplot');
  setMPvalues(spec.parent.object,plotInfo);
  plotInfo.xtop=false;
  plotInfo.yright=false;
  plotInfo.autox=1;
  plotInfo.autoy=1;
  plotInfo.hideRectangle=false;
  setValues(viewSpec,true);
 };
 function firstPlot(active,ps)
 {
  multiPos=-1;
  multiPlots.firstPlot=0;
  setMPvalues(plotInfo,memInfo,true);
  memCalc=plotCalc;
  xorg=plotOptions.plotPosX;
  yorg=plotOptions.plotPosY+yoffs;
  worg=plotOptions.plotWidth;
  horg=plotOptions.plotHeight;
  plotOptions.key=false;
  multiSpec.emptyContainer();
  return nextPlot(active,ps,true);
 };
 function finalPlot()
 {
  if(plotOptions.key || (!multiPlots.showKey))
  {
   setMPvalues(memInfo,plotInfo,true);
   plotCalc=memCalc;
   plotOptions.plotPosX=xorg;
   plotOptions.plotPosY=yorg-yoffs;
   plotOptions.plotWidth=worg;
   plotOptions.plotHeight=horg;
   multiSpec.fillContainer();
   return false;
  }
  else
  {
   plotOptions.keyPosX=xorg+multiPlots.keyColumn*plotOptions.cellWidth;
   plotOptions.keyPosY+=yoffs+(plotOptions.maxrow-multiPlots.keyRow)*
    plotOptions.cellHeight;
   plotOptions.key=true;
  };
  return true;
 };
 function nextPlot(active,ps,firstTrial)
 {
  if((multiPos>-1)&&(multiPos<multiPlots.plots.length))
  {
   setMPvalues(plotInfo,multiPlots.plots[multiPos],true,true);
  };
  multiPos++;
  if(multiPos>multiPlots.plots.length-1){return finalPlot();};
  while((multiPos<multiPlots.plots.length-1) && (!multiPlots.plots[multiPos].selected))
  {
   multiPos++;
  };
  if(!multiPlots.plots[multiPos].selected){return finalPlot();};
  if(firstTrial)
  {
   multiPlots.firstPlot=multiPos;
  };
  setMPvalues(multiPlots.plots[multiPos],plotInfo,firstTrial);
  if((multiPlots.tiedXAxes>1)&&(multiPlots.plots[multiPos].row!=plotOptions.maxrow))
  {
   if(multiPlots.plots[multiPos].row==0)
   {
    plotInfo.xtop=true;
   }
   else
   {
    plotInfo.xlabel="";
    if(multiPlots.tiedXAxes==3){plotInfo.xlabel="nonumbers";};
    if((multiPlots.tiedXAxes==4)||((multiPlots.tiedXAxes==3) && multiPlots.plots[multiPos].hideRectangle)){plotInfo.xlabel="noaxis";};
   };
  };
  if((multiPlots.tiedYAxes>1)&&(multiPlots.plots[multiPos].column>0))
  {
   plotInfo.ylabel="";
   if(multiPlots.tiedYAxes==3){plotInfo.ylabel="nonumbers";};
   if((multiPlots.tiedYAxes==4)||((multiPlots.tiedYAxes==3) && multiPlots.plots[multiPos].hideRectangle)){plotInfo.ylabel="noaxis";};
  };
  plotOptions.plotPosX=xorg+multiPlots.plots[multiPos].column*plotOptions.cellWidth;
  plotOptions.plotPosY=yorg+
   (plotOptions.maxrow-multiPlots.plots[multiPos].row)*
   plotOptions.cellHeight;
  if(multiPlots.plots[multiPos].colspan)
  {
   plotOptions.plotWidth=worg+(multiPlots.plots[multiPos].colspan-1)*plotOptions.cellWidth;
  }
  else
  {
   plotOptions.plotWidth=worg;
  };
  if(multiPlots.plots[multiPos].rowspan)
  {
   plotOptions.plotHeight=horg+(multiPlots.plots[multiPos].rowspan-1)*plotOptions.cellHeight;
   plotOptions.plotPosY-=(multiPlots.plots[multiPos].rowspan-1)*plotOptions.cellHeight;
  }
  else
  {
   plotOptions.plotHeight=horg;
  };
  if(!active)
  {
   calcData(true);
   plotCalcMulti[multiPos]=plotCalc;
  }
  else
  {
   plotCalc=plotCalcMulti[multiPos];
  };
  return true;
 };
 function arrangePlots()
 {
  var i,j;
  multiSpec.emptyContainer();
  j=0;
  if(multiPlots.columns==0){return;};
  for(i=0;i<multiPlots.plots.length;i++)
  {
   if(multiPlots.plots[i].selected)
   {
    multiPlots.plots[i].row=Math.floor(j/multiPlots.columns);
    multiPlots.plots[i].column=j % multiPlots.columns;
    j++;
   }
   else
   {
    multiPlots.plots[i].row=0;
    multiPlots.plots[i].column=0;
   };
  };
  multiPlots.keyRow=Math.floor(j/multiPlots.columns);
  multiPlots.keyColumn=j % multiPlots.columns;
  multiSpec.fillContainer();
 };
 function rangeFinder(obj)
 {
  obj.autox=0;obj.autoy=0;
  return "("+obj.x_calc+" BETWEEN "+obj.minx+" AND "+obj.maxx+") AND ("+
   obj.y_calc+" BETWEEN "+obj.miny+" AND "+obj.maxy+")";
 };
 function findInRange(spec)
 {
  var i,tbl;
  var str="";
  spec.parent.emptyContainer();
  if(spec.parent.object.x_calc)
  {
   str=rangeFinder(spec.parent.object);
  }
  else
  {
   if(spec.parent.object.plots)
   {
    for(i=0;i<spec.parent.object.plots.length;i++)
    {
     if(spec.parent.object.plots[i].selected)
     {
      if(str){str+=" AND ";};
      str+=rangeFinder(spec.parent.object.plots[i]);
     };
    };
    str="("+str+")";
   };
  };
  spec.parent.fillContainer();
  if(editOptions.window)
  {
   if(!editOptions.window.closed)
   {
    tbl=-1;
    for(i=0;i<editOptions.window.database.views.length;i++)
    {
     if(plotInfo.table==editOptions.window.database.views[i].view)
     {
      tbl=i;
     };
    };
    if(tbl>-1)
    {
     str=editOptions.window.location.href.split("?")[0]+
       "?page=search&auto=true&view="+tbl+"&where="+str;
     editOptions.window.location.assign(str);
     editOptions.window.focus();
    };
   };
  };
 };
 function initDataArea()
 {
   if(editOptions.window)
   {
    if(!editOptions.window.closed)
    {
//     editOptions.window=window.opener;
     if(editOptions.window.plotInfo)
     {
      plotInfo=duplRemoteItem(editOptions.window,editOptions.window.plotInfo);
     };
     if(editOptions.window.plotColor)
     {
      plotColor=duplRemoteItem(editOptions.window,editOptions.window.plotColor);
     };
     if(editOptions.window.plotOptions && editOptions.window.plotOptions.plotPosX)
     {
      plotOptions.plotPosX=editOptions.window.plotOptions.plotPosX;
      plotOptions.plotPosY=editOptions.window.plotOptions.plotPosY;
      plotOptions.plotWidth=editOptions.window.plotOptions.plotWidth;
      plotOptions.plotHeight=editOptions.window.plotOptions.plotHeight;
      plotOptions.multiPlot=editOptions.window.plotOptions.multiPlot;
      if(typeof(editOptions.window.plotOptions.showKey)!='undefined')
      {
       plotOptions.showKey=editOptions.window.plotOptions.showKey;
      };
      if(typeof(editOptions.window.plotOptions.contourProbMax)!='undefined')
      {
       plotOptions.contourProbMax=editOptions.window.plotOptions.contourProbMax;
      };
     };
     if(editOptions.window.plotData)
     {
      plotData=duplRemoteItem(editOptions.window,editOptions.window.plotData);
     };
     if(editOptions.window.multiPlots)
     {
      multiPlots=duplRemoteItem(editOptions.window,editOptions.window.multiPlots);
     };
     if(editOptions.window.player && !editOptions.window.playerDisable)
     {
      document.getElementById("toolPlayer").style.display="block";
     };
    }
    else
    {
     editOptions.window=false;
    };
   }
   else
   {
    editOptions.window=false;
   };
   canvas=decodeObject(canvas);
   plotInfo=decodeObject(plotInfo);
   plotOptions=decodeObject(plotOptions);
   plotData=decodeObject(plotData);
   multiPlots=decodeObject(multiPlots);
   plotColor=decodeObject(plotColor);
   document.getElementById("importArea").appendChild(editSpec.createDisplay(editOptions));   
   document.getElementById("viewPlotArea").appendChild(viewSpec.createDisplay(plotInfo));   
   document.getElementById("viewColorArea").appendChild(colorSpec.createDisplay(plotColor));   
   document.getElementById("formatArea").appendChild(formatSpec.createDisplay(plotOptions));   
   document.getElementById("canvasOptionsArea").appendChild(canvasSpec.createDisplay(canvas));   
   document.getElementById("fileOptionsArea").appendChild(fileSpec.createDisplay(fileOptions));   
   document.getElementById("viewMultiArea").appendChild(multiSpec.createDisplay(multiPlots));   
   document.getElementById("dataArea").appendChild(dataSpec.createDisplay(plotData));
   setDataTables(true);
   window.onfocus=changeEditOptions;
   if(args.source)
   {
    setFilename(args.source);
    document.getElementById('fileFrame').src="oxplot_load.html";
   }
   else
   {
    setFilename("Untitled.plot");
   };
 };
 function goSearch()
 {
  editOptions.window.focus();
 };
 function datSort(a,b)
 {
  if(a[editOptions.division]>b[editOptions.division]){return 1;};
  if(a[editOptions.division]<b[editOptions.division]){return -1;};
  return 0;
 };
 function parseTabData(txt,div)
 {
  var dat,lines,items,headers,i,j,txts,nos,k;
  dat=new Array();
  k=0;
  lines=txt.split("\n");
  for(i=0;i<lines.length;i++)
  {
   items=lines[i].split("\t");
   txts=0;nos=0;
   for(j=0;j<items.length;j++)
   {
    if(items[j]!="")
    {
     if(isNaN(items[j])){txts++;}else{nos++;};
    };
   };
   if(((nos==0)&&(txts>1))||(i==0))
   {
    headers=items;
   }
   else
   {
    if(nos==0){continue;};
    dat[k]=new Object();
    for(j=0;j<items.length;j++)
    {
     if(!headers[j]){continue;};
     if((items[j]=="")||isNaN(items[j]))
     {
      switch(items[j])
      {
      case "true":
       dat[k][headers[j]]=true;
       break;
      case "false":
       dat[k][headers[j]]=false;
       break;
      default:
       dat[k][headers[j]]=items[j];
       break;
      };
     }
     else
     {
      dat[k][headers[j]]=Number(items[j]);
     };
    };
    k++;
   };
  };
  if(div)
  {
   dat.sort(datSort);
  };
  return dat;
 };
 function suckData(dta)
 {
  var i,j,newData;
  dataSpec.emptyContainer();
  if(dta)
  {
   newData=dta;
  }
  else
  {
   editSpec.refillContainer();
   if(tables[editOptions.table]=="[Text]")
   {
    newData=parseTabData(editOptions.text,editOptions.division);
   }
   else
   {
    if(editOptions.window.thisPage.search.views)
    {
     if(editOptions.window.thisPage.search.views[editOptions.table])
     {
      plotInfo.table=editOptions.window.thisPage.search.views[editOptions.table].view;
     };
     plotInfo.category=editOptions.window.dbDataSpec[editOptions.table].prompt;
     if(!editOptions.division)
     {
      for(i=0;i<editOptions.window.dbData[editOptions.table].length;i++)
      {
       if(editOptions.window,editOptions.window.dbData[editOptions.table][i].selected)
       {
        editOptions.division='selected';
       };
      };
     };
    };
    if(editOptions.division)
    {
     editOptions.window.dbDataSpec[editOptions.table].sortArray(editOptions.division,true);
    };
    newData=duplRemoteItem(editOptions.window,editOptions.window.dbData[editOptions.table]);
   };
   if(editOptions.division)
   {
    j=0;
    for(i=1;i<newData.length;i++)
    {
     if(newData[i][editOptions.division]!=newData[i-1][editOptions.division])
     {
      if(!((editOptions.division=='selected')&&(!newData[j][editOptions.division])))
      {
       suckData(newData.slice(j,i));
      };
      j=i;
     };
    };
    if(!((editOptions.division=='selected')&&(!newData[j][editOptions.division])))
    {
     suckData(newData.slice(j));
    };
    return;
   };
  };
  i=plotData.push(new Object())-1;
  plotData[i].data=newData;
  if(plotData[i].data && plotData[i].data.length)
  {
   if(plotData[i].data[0].url)
   {
    plotData[i].include_url=true;
   };
   if(plotData[i].data[0].id)
   {
    plotData[i].include_id=true;
   };
   if(plotData[i].data[0].marker)
   {
    plotData[i].include_marker=true;
   };
   if(plotData[i].data[0].color)
   {
    plotData[i].include_color=true;
   };
  };
  plotData[i].name="";
  if(tables[editOptions.table]=="[Text]")
  {
   plotData[i].name=editOptions.title;
  }
  else
  {
   if(editOptions.window.dbPlotKey)
   {
    plotData[i].name=editOptions.window.dbPlotKey;
   }
   else
   {
    for(j=0;j<editOptions.window.args.length;j++)
    {
     switch(editOptions.window.args[j].name)
     {
     case "auto":case "page": break;
     default:
      if(plotData[i].name){plotData[i].name+="/"};
      plotData[i].name+=editOptions.window.args[j].value;
     };
    };
   };
  };
  if(dta && editOptions.division)
  {
   if(plotData[i].name){plotData[i].name+=", ";};
   switch(typeof(dta[0][editOptions.division]))
   {
   case "undefined":
   case "boolean":
    if(dta[0][editOptions.division])
    {
     plotData[i].name+=editOptions.division;
    }
    else
    {
     plotData[i].name+="NOT "+editOptions.division;
    };
    break;
   default:
    plotData[i].name+=dta[0][editOptions.division];
    break;
   };
  };
  plotData[i].marker=markers[(dataCount % 6)+1];
  if(plotOptions.BandW)
  {
   if(darkBackground())
   {
    plotData[i].markerColor="rgb(255,255,255)";
   }
   else
   {
    plotData[i].markerColor="rgb(0,0,0)";
   };
   switch(Math.floor(dataCount / 6))
   {
   case 0:
    plotData[i].markerFill=plotData[i].markerColor.replace( "rgb","rgba").replace(")",",0)");
    break;
   case 1:
    plotData[i].markerFill=plotData[i].markerColor.replace( "rgb","rgba").replace(")",",0.5)");
    break;
   default:
    plotData[i].markerFill=plotData[i].markerColor.replace( "rgb","rgba").replace(")",",1)");
    break;
   };
   plotData[i].line="";
   plotData[i].lineColor=plotData[i].markerColor;
  }
  else
  {
   plotData[i].markerColor=colors[(dataCount % (colors.length))];
   plotData[i].markerFill=plotData[i].markerColor.replace( "rgb","rgba").replace(")",",0.5)");
   plotData[i].line="";
   plotData[i].lineColor=colors[(dataCount % (colors.length))];
  };
  plotData[i].selected=true;
  dataCount++;
  dataSpec.fillContainer();
  setValues(dataSpec);
 };
 function suckPlotInformation()
 {
  var i,j,dta,skip,thisId;
  if(editOptions.window)
  {
   if(!editOptions.window.closed)
   {
    if(editOptions.window.plotData)
    {
     dataSpec.emptyContainer();
     for(i=0;i<editOptions.window.plotData.length;i++)
     {
      autofill=false; // don't also import data
      skip=false;
      thisId=editOptions.window.plotData[i].id;
      if(thisId)
      {
       for(j=0;j<plotData.length;j++)
       {
        if(plotData[j].id==thisId){skip=true;};
       };
      };
      if(!skip)
      {
       plotData.push(duplRemoteItem(editOptions.window,editOptions.window.plotData[i]));
      };
     };
     dataSpec.fillContainer();
     setValues(dataSpec);
    };
/*    if(editOptions.window.plotInfo)
    {
     plotInfo=duplRemoteItem(editOptions.window,editOptions.window.plotInfo);
    };
    if(editOptions.window.plotOptions)
    {
     formatSpec.emptyContainer();
     plotOptions.plotPosX=editOptions.window.plotOptions.plotPosX;
     plotOptions.plotPosY=editOptions.window.plotOptions.plotPosY;
     plotOptions.plotWidth=editOptions.window.plotOptions.plotWidth;
     plotOptions.plotHeight=editOptions.window.plotOptions.plotHeight;
     plotOptions.multiPlot=editOptions.window.plotOptions.multiPlot;
     formatSpec.fillContainer();
    };
    if(editOptions.window.multiPlots)
    {
     multiSpec.emptyContainer();
     multiPlots.showKey=editOptions.window.multiPlots.showKey;
     multiPlots.keyRow=editOptions.window.multiPlots.keyRow;
     multiPlots.keyColumn=editOptions.window.multiPlots.keyColumn;
     multiPlots.columns=editOptions.window.multiPlots.columns;
     multiPlots.plots=duplRemoteItem(editOptions.window,editOptions.window.multiPlots.plots);
     multiSpec.fillContainer();
    };*/
   };
  };
 };
 function out_of_range(v,min,max,auto)
 {
  if(auto!=0){return false;};
  if(max>min)
  {
   if((v>1.05*max-0.05*min)||(v<1.05*min-0.05*max)){return true;};
  }
  else
  {
   if((v>1.05*min-0.05*max)||(v<1.05*max-0.05*min)){return true;};
  };
  return false;
 };
 function BC(t){return 1-t;};
 function BCE(t){return 1-t;};
 function AD(t){return t;};
 function CE(t){return t;};
 function b2k(t){return 2000-t;};
 function calBP(t){return 1950.5-t;};
 function G(t){return t;};
 function m(z){return z;};
 function cm(z){return z*100;};
 function mm(z){return z*1000;};
 function pointCalculator(data_set,multi)
 {
  var str;var id="";var url="";
  var obj={fault:false,dx_fault:false,dy_fault:false,z_fault:false,
   x_val:false,y_val:false,dx_val:false,dy_val:false,z_val:false};
  var divider=true;var c,cno,itemname,data_point;
  plotCalc[data_set]=new Object();
  plotCalc[data_set].x=new Array;
  plotCalc[data_set].y=new Array;
  plotCalc[data_set].dx=new Array;
  plotCalc[data_set].dy=new Array;
  plotCalc[data_set].xc=new Array;
  plotCalc[data_set].yc=new Array;
  plotCalc[data_set].w=new Array;
  plotCalc[data_set].dm=new matrix(0,2);
  if(plotData[data_set].include_id || plotData[data_set].include_url || plotData[data_set].include_marker || plotData[data_set].include_color  || plotData[data_set].include_label || plotData[data_set].marker=='label' || plotData[data_set].marker=='image')
  {
   plotCalc[data_set].index=new Array;
  }
  else
  {
   plotCalc[data_set].index=false;
  };
  if(plotColor.z_calc)
  {
   plotCalc[data_set].z=new Array;
   if(plotColor.dz_calc)
   {
    plotCalc[data_set].dz=new Array;
   };
  };
  if(multi && plotData[data_set].singleOnly){return;};
  if(!plotData[data_set].selected){return;};
  function valVal(item)
  {
   switch(item.constructor)
   {
   case Number: return item; break;
   case String: return item.replace(/'/g,"\\'"); break;
   };
   return false;
  };
  str="function getVals(obj,data_set,data_point){";
  for(itemname in plotData[data_set].data[0])
  {
   str+="try{var "+itemname.replace(/[^A-Za-z0-9]/g,"_")+
    "=valVal(plotData[data_set].data[data_point]['"+itemname+"']);}catch(err){};";
  };
  if(plotInfo.x_calc)
  {
   str+="try{";
   str+="obj.x_val="+plotInfo.x_calc+";";
   str+="}catch(err){obj.fault=true;};";
  };
  if(plotInfo.dx_calc)
  {
   str+="try{";
   str+="obj.dx_val="+plotInfo.dx_calc+";";
   str+="}catch(err){obj.dx_fault=true;};";
  };
  if(plotInfo.y_calc)
  {
   str+="try{";
   str+="obj.y_val="+plotInfo.y_calc+";";
   str+="}catch(err){obj.fault=true;};";
  };
  if(plotInfo.dy_calc)
  {
   str+="try{";
   str+="obj.dy_val="+plotInfo.dy_calc+";";
   str+="}catch(err){obj.dy_fault=true;};";
  };
  if(plotColor.z_calc)
  {
   str+="try{";
   str+="obj.z_val="+plotColor.z_calc+";";
   str+="}catch(err){obj.z_fault=true;};";
  };
  if(plotColor.dz_calc)
  {
   str+="try{";
   str+="obj.dz_val="+plotColor.dz_calc+";";
   str+="}catch(err){obj.dz_fault=true;};";
  };
  if(plotInfo.weight)
  {
   str+="try{";
   str+="obj.w_val="+plotInfo.weight+";";
   str+="}catch(err){obj.dz_fault=true;};";
  };
  str+="};";
  try
  {
   eval(str);
  }
  catch(err)
  {
   alert("Problem with using item names:\n\n"+str);
  };
  for(data_point=0;data_point<plotData[data_set].data.length;data_point++)
  {
   obj.fault=false;obj.dx_fault=false;obj.dy_fault=false;obj.z_fault=false;obj.dz_fault=false;
   try
   {
    getVals(obj,data_set,data_point);
   }
   catch(err)
   {
    obj.fault=true;
   };
   if(!obj.fault)
   {
    if(plotInfo.x_calc)
    {
     if(out_of_range(obj.x_val,plotInfo.minx,plotInfo.maxx,plotInfo.autox) &&  !plotInfo.weight){obj.fault=true;};
     if(isNaN(obj.x_val)){obj.fault=true;};
    };
    if(plotInfo.y_calc)
    {
     if(out_of_range(obj.y_val,plotInfo.miny,plotInfo.maxy,plotInfo.autoy) &&  !plotInfo.weight){obj.fault=true;};
     if(isNaN(obj.y_val)){obj.fault=true;};
    };
    if(plotColor.z_calc)
    {
     if(isNaN(obj.z_val)){obj.z_fault=true;};
    };
    if(plotInfo.weight)
    {
     if(isNaN(obj.w_val)){obj.w_fault=true;};
    };
   };
   if(!obj.fault)
   {
    if(plotCalc[data_set].index)
    {
     plotCalc[data_set].index.push(data_point);
    };
    if(plotInfo.x_calc)
    {
     plotCalc[data_set].x.push(obj.x_val);
     if(plotInfo.y_calc)
     {
      plotCalc[data_set].dm.rows++;
      plotCalc[data_set].dm.m.push([obj.x_val,obj.y_val]);
     };
    };
    if(plotInfo.dx_calc)
    {
     if(obj.dx_fault)
     {
      plotCalc[data_set].dx.push(0);
     }
     else
     {
      plotCalc[data_set].dx.push(obj.dx_val);
     };
    };
    if(plotInfo.y_calc)
    {
     plotCalc[data_set].y.push(obj.y_val);
    };
    if(plotInfo.dy_calc)
    {
     if(obj.dy_fault)
     {
      plotCalc[data_set].dy.push(0);
     }
     else
     {
      plotCalc[data_set].dy.push(obj.dy_val);
     };
    };
    if(plotColor.z_calc)
    {
     if(obj.z_fault)
     {
      plotCalc[data_set].z.push('NaN');
     }
     else
     {
      plotCalc[data_set].z.push(obj.z_val);
     };
    };
    if(plotColor.dz_calc)
    {
     if(obj.dz_fault)
     {
      plotCalc[data_set].dz.push('NaN');
     }
     else
     {
      plotCalc[data_set].dz.push(obj.dz_val);
     };
    };
    if(plotInfo.weight)
    {
     if(obj.w_fault)
     {
      plotCalc[data_set].w.push('NaN');
     }
     else
     {
      plotCalc[data_set].w.push(obj.w_val);
     };
    };
    divider=false;
   }
   else
   {
    if(!divider && !plotInfo.weight) // don't put in divider for kde type data
    {
     if(plotCalc[data_set].index)
     {
      plotCalc[data_set].index.push(data_point);
     };
     if(plotInfo.x_calc)
     {
      plotCalc[data_set].x.push('NaN');
     };
     if(plotInfo.dx_calc)
     {
      plotCalc[data_set].dx.push('NaN');
     };
     if(plotInfo.y_calc)
     {
      plotCalc[data_set].y.push('NaN');
     };
     if(plotInfo.dy_calc)
     {
      plotCalc[data_set].dy.push('NaN');
     };
     if(plotColor.z_calc)
     {
      plotCalc[data_set].z.push('NaN');
     };
     if(plotColor.dz_calc)
     {
      plotCalc[data_set].dz.push('NaN');
     };
     if(plotInfo.weight)
     {
      plotCalc[data_set].w.push('NaN');
     };
     divider=true;
    };
   };
  };
  if(plotCalc[data_set].dm.rows<2){return;};
  if(plotData[data_set].contour)
  {
   plotCalc[data_set].kde=new kde(plotCalc[data_set].dm.rows,
   plotCalc[data_set].dm.columns,
   plotCalc[data_set].dm.m,plotCalc[data_set].w);
   if(plotOptions.contours=="95%")
   {
    cno=0;
   }
   else
   {
    if(plotOptions.contourProbMax)
    {
     cno=2*Number(plotOptions.contours)*plotCalc[data_set].kde.data.sumWeights
     	/plotOptions.contourProbMax;
     if(cno<1){cno=0.5;}; // no contours rather than 95%
    }
    else
    {
     cno=Number(plotOptions.contours);
    };
   };
  };
  switch(plotData[data_set].contour)
  {
  case "Ellipse":
   plotCalc[data_set].kde.ellipse_2d_setup(0,1);
   c=plotCalc[data_set].kde.ellipse_2d(0,1,cno,100);
   plotCalc[data_set].xc=c.m[0];
   plotCalc[data_set].yc=c.m[1];
   break;
  case "KDE":
   plotCalc[data_set].kde.kde_2d_setup(0,1);
   c=plotCalc[data_set].kde.kde_contour_2d(0,1,cno,40);
   plotCalc[data_set].xc=c.m[0];
   plotCalc[data_set].yc=c.m[1];
   break;
  default:
   break;
  };
 };
 function calcData(multi)
 {
  var i;
  plotCalc=new Array();
  for(i=0;i<plotData.length;i++)
  {
   pointCalculator(i,multi);
  };
 };
 function finishPlot()
 {
  if(editOptions.window)
  {
   if(!editOptions.window.closed)
   {
    if(editOptions.window.finishPlot)
    {
     editOptions.window.finishPlot();
    };
   }
  };
 };
 var dataEditSpec;
 function makeArraySpec(ar)
 {
  var spec=new itemSpec("data","Data","Array");
  spec.database=true;
  if(ar.length && ar.length>0)
  {
   for(var el in ar[0])
   {
    switch(el)
    {
    case "deleted":
    case "selected":
    case "changed":
    case "created":
    case "db_sort_line":
     break;
    default:
     switch(typeof(ar[0][el]))
     {
     case "boolean":
      spec.appendChild(el,el,"Boolean");
      break;
     case "number":
      spec.appendChild(el,el,"Number");
      break;
     case "string":
      spec.appendChild(el,el,"Text");
      break;
     };
     break;
    };
   };
  };
  return spec;
 };
 function putEditArray(ar)
 {
  dataEditSpec=makeArraySpec(ar);
  document.getElementById("dataEditArea").removeChild( document.getElementById("dataEditArea").firstChild);
  document.getElementById("dataEditArea").appendChild(dataEditSpec.createDisplay(ar));   
 };
 function getEditArrayText()
 {
  return dataEditSpec.exportData(true,"csv");
 };
 function startEditData(spec)
 {
  putEditArray(spec.parent.object.data);
  showArea("editEditData");
 };
 function returnEditData()
 {
  cleanDatabaseArray(dataEditSpec,dataEditSpec.object);
 };
 function exportEditData()
 {
  filename="Untitled.csv";
  filecontent=dataEditSpec.exportData(true,"csv");
  saveFileAs("csv");
  hideArea('fileOptionsArea');
  showArea('fileDialog');
 };
 function cancelEditData()
 {
  dataEditSpec.refillContainer(true);
 };
 function saveAllData()
 {
  var i;
  filename="Untitled.csv";
  filecontent="";
  for(i=0;i<plotData.length;i++)
  {
   if(plotData[i].selected)
   {
    putEditArray(plotData[i].data);
    filecontent+=dataEditSpec.exportData(true,"csv")+"\n";
   };
  };
  saveFileAs("csv");
  hideArea('fileOptionsArea');
  showArea('fileDialog');
};
 
 var covMatrix=new matrix();
 var evMatrix=new matrix();

 function doPCA()
 {
  var i,j,k,ok;
  var index=new Array;
  var row,indrow;
  plotInfo.pcaVariables=prompt("Variables",plotInfo.pcaVariables);
  var items=plotInfo.pcaVariables.split(",");
  var dta=new matrix(0,items.length);
  // grab all of the data and index it
  for(i=0;i<plotData.length;i++)
  {
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   for(j=0;j<plotData[i].data.length;j++)
   {
    row=new Array();
    indrow=new Array(i,j);
    ok=true;
    for(k=0;k<items.length;k++)
    {
     if(isNaN(plotData[i].data[j][items[k]]))
     {
      ok=false;
     }
     else
     {
      row[k]=Number(plotData[i].data[j][items[k]]);
     };
    };
    if(ok)
    {
     dta.m.push(row);
     dta.rows++;
     index.push(indrow);
    };
   };
  };
  // now do the PCA
  var dn=dta.normalise();
  covMatrix=dn.covariance();
  evMatrix=covMatrix.eigenvectors().sort_eigenvalues();
  var tr=evMatrix.transpose();
  var pcadta=dn.transpose().times(tr).transpose().normalise();
  for(i=0;i<pcadta.rows;i++)
  {
   for(j=0;j<pcadta.columns;j++)
   {
    plotData[index[i][0]].data[index[i][1]]["PC"+(j+1)]=pcadta.m[i][j];
   };
  };
  plotInfo.x_calc="PC1";
  plotInfo.y_calc="PC2";
  plotInfo.xlabel="PC1";
  plotInfo.ylabel="PC2";
  plotInfo.title="PCA";
  covMatrix.rowNames=items;
  covMatrix.colNames=items;
  evMatrix.rowNames=items;
  evMatrix.colNames=new Array();
  for(i=0;i<items.length;i++)
  {
   evMatrix.colNames[i]="PC"+(i+1);
  };
  document.getElementById("covMatrixArea").removeChild( document.getElementById("covMatrixArea").firstChild);
  document.getElementById("covMatrixArea").removeChild( document.getElementById("covMatrixArea").firstChild);
  document.getElementById("covMatrixArea").appendChild(covMatrix.display(5,'object'));
  document.getElementById("covMatrixArea").appendChild(evMatrix.display(5,'object'));
 };
 function exportCovMatrix()
 {
  filecontent=covMatrix.output(5,true);
  filecontent+=evMatrix.output(5,true);
  filename="Untitled.csv";
  saveFileAs("csv");
  showArea('fileDialog');
 };
 function zoom(no)
 {
  canvas.scale*=no;
  if(canvas.scale==0){canvas.scale=1;};
  setValues(canvasSpec,true);
 };
 function centrePlotInf(pi)
 {
  if(!pi.autox){pi.autox=1;};
  if(!pi.autoy){pi.autoy=1;};
 };
 function changeMinXInf(pi,no)
 {
  var ch;
  ch=no*(pi.maxx-pi.minx);
  pi.minx+=ch;
  pi.maxx+=ch;
  pi.autox=0;
 };
 function changeMinYInf(pi,no)
 {
  var ch;
  ch=no*(pi.maxy-pi.miny);
  pi.miny+=ch;
  pi.maxy+=ch;
  pi.autoy=0;
 };
 function changeXRangeInf(pi,no,only)
 {
  var ch;
  ch=(no-1)*(pi.maxx-pi.minx);
  if(pi.minx==0)
  {
   pi.maxx+=ch;
  }
  else
  {
   pi.minx-=ch/2;
   pi.maxx+=ch/2;
  };
  pi.autox=0;
  if(pi.mapPlot && !only)
  {
   changeYRangeInf(pi,no,true);
  };
 };
 function changeYRangeInf(pi,no,only)
 {
  var ch;
  ch=(no-1)*(pi.maxy-pi.miny);
  if(pi.miny==0)
  {
   pi.maxy+=ch;
  }
  else
  {
   pi.miny-=ch/2;
   pi.maxy+=ch/2;
  };
  pi.autoy=0;
  if(pi.mapPlot && !only)
  {
   changeXRangeInf(pi,no,true);
  };
 };
 function singlePlotAction(func,gr,no)
 {
  var ch;
  if(gr=="ColorScale")
  {
   switch(func)
   {
   case changeYRangeInf: 
    ch=(no-1)*(plotColor.maxz-plotColor.minz);
    if(plotColor.minz==0)
    {
     plotColor.maxz+=ch;
    }
    else
    {
     plotColor.minz-=ch/2;
     plotColor.maxz+=ch/2;
    };
    plotColor.autoz=0;
    break;
   case changeMinYInf:
    ch=no*(plotColor.maxz-plotColor.minz);
    plotColor.minz+=ch;
    plotColor.maxz+=ch;
    plotColor.autoz=0;
    break;
   };
   setValues(colorSpec,true);
   return;
  };
  switch(func)
  {
  case changeYRangeInf: case changeMinYInf:
   if(multiPlots.tiedYAxes){gr=multiPlots.firstPlot;};break;
  case changeXRangeInf: case changeMinXInf:  
   if(multiPlots.tiedXAxes){gr=multiPlots.firstPlot;};break;
  };
  multiSpec.emptyContainer();
  func(multiPlots.plots[gr],no);
  multiSpec.fillContainer();
  setValues(multiSpec,true);
 };
 function plotAction(func,no)
 {
  var i;
  if(plotOptions.multiPlot)
  {
   multiSpec.emptyContainer();
   for(i=0;i<multiPlots.plots.length;i++)
   {
    func(multiPlots.plots[i],no);
   };
   multiSpec.fillContainer();
   setValues(multiSpec,true);
  }
  else
  {
   func(plotInfo,no);
   setValues(viewSpec,true);
  };
 };
 function centrePlot()
 {
  plotAction(centrePlotInf);
 };
 function changeMinX(no)
 {
  plotAction(changeMinXInf,no);
 };
 function changeMinY(no)
 {
  plotAction(changeMinYInf,no);
 };
 function changeXRange(no)
 {
  plotAction(changeXRangeInf,no);
 };
 function changeYRange(no)
 {
  plotAction(changeYRangeInf,no);
 };
 function smallerFont()
 {
  canvas.scaleFont/=1.0443;
  if(!canvas.scaleFont){canvas.scaleFont=0.8;};
  setValues(canvasSpec,true);
 };
 function largerFont()
 {
  canvas.scaleFont*=1.0443;
  if(!canvas.scaleFont){canvas.scaleFont=0.8;};
  setValues(canvasSpec,true);
 };
 function narrowerFont()
 {
  canvas.scaleLine/=1.0443;
  if(!canvas.scaleLine){canvas.scaleLine=1.0;};
  setValues(canvasSpec,true);
 };
 function widerFont()
 {
  canvas.scaleLine*=1.0443;
  if(!canvas.scaleLine){canvas.scaleLine=1.0;};
  setValues(canvasSpec,true);
 };
 function toggleMarkers()
 {
  var i,no,state;
  dataSpec.emptyContainer();
  for(no=0,i=0;i<plotData.length;i++)
  {
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   if(no==0)
   {
    state=(plotData[i].marker=="");
   };
   if(state)
   {
    if(plotData[i].mem_marker)
    {
     plotData[i].marker=plotData[i].mem_marker;
    }
    else
    {
     plotData[i].marker=markers[no % 6 + 1];
    };
   }
   else
   {
    plotData[i].mem_marker=plotData[i].marker;
    plotData[i].marker="";
   };
   no++;
  };
  dataSpec.fillContainer();
  setValues(dataSpec);
 };
 function toggleLines()
 {
  var i,no,state;
  dataSpec.emptyContainer();
  for(no=0,i=0;i<plotData.length;i++)
  {
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   if(no==0)
   {
    state=(plotData[i].line=="");
   };
   if(state)
   {
    if(plotData[i].mem_line)
    {
     plotData[i].line=plotData[i].mem_line;
    }
    else
    {
     plotData[i].line="solid";
    };
   }
   else
   {
    plotData[i].mem_line=plotData[i].line;
    plotData[i].line="";
   };
   no++;
  };
  dataSpec.fillContainer();
  setValues(dataSpec);
 };
 function toggleEllipses()
 {
  var i,no,state;
  dataSpec.emptyContainer();
  for(no=0,i=0;i<plotData.length;i++)
  {
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   if(no==0)
   {
    state=(plotData[i].contour!="Ellipse");
   };
   if(state)
   {
    plotData[i].contour="Ellipse";
   }
   else
   {
    plotData[i].contour="";
   };
   no++;
  };
  dataSpec.fillContainer();
  setValues(dataSpec);
 };
 function toggleKDEContours()
 {
  var i,no,state;
  dataSpec.emptyContainer();
  for(no=0,i=0;i<plotData.length;i++)
  {
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   if(no==0)
   {
    state=(plotData[i].contour!="KDE");
   };
   if(state)
   {
    plotData[i].contour="KDE";
   }
   else
   {
    plotData[i].contour="";
   };
   no++;
  };
  dataSpec.fillContainer();
  setValues(dataSpec);
 };
 function setUpMatrix(variables)
 {
  var i,j,k,l,ok;
  var row,m;
  variables=prompt("Variables",variables);
  var items=variables.split(",");
  var matrixData=new Array();
  var rowNames=new Array();
  var colNames=new Array();
  l=0;
  // grab all of the data and index it
  for(i=0;i<plotData.length;i++)
  {
   var dta=new matrix(0,items.length);
   if(!plotData[i].selected){continue;};
   if(plotData[i].singleOnly){continue;};
   for(j=0;j<plotData[i].data.length;j++)
   {
    row=new Array();
    ok=true;
    for(k=0;k<items.length;k++)
    {
     if(isNaN(plotData[i].data[j][items[k]]))
     {
      ok=false;
     }
     else
     {
      row[k]=Number(plotData[i].data[j][items[k]]);
     };
    };
    if(ok)
    {
     dta.m.push(row);
     dta.rows++;
    };
   };
   if(dta.rows<2){continue;};
   plotCalc[i].fullKDE=new kde(dta.rows,dta.columns,dta.m);
   plotCalc[i].fullKDE.pca_setup();
   matrixData.push(i);
   if(l<26)
   {
    rowNames.push(String.fromCharCode(65+l)+" - "+plotData[i].name);
    colNames.push(String.fromCharCode(65+l));
   }
   else
   {
    rowNames.push(String.fromCharCode(65+Math.floor(l/26),65+l%26)+" - "+plotData[i].name);
    colNames.push(String.fromCharCode(65+Math.floor(l/26),65+l%26));
   };
   l++;
  };
  m=new matrix(matrixData.length,matrixData.length);
  m.matrixData=matrixData;
  m.rowNames=rowNames;
  m.colNames=colNames;
  m.variables=variables;
  return m;
 };
 function bivariateList()
 {
  if(plotInfo.title=="PCA"){return "PC1,PC2";};
  var items=plotInfo.pcaVariables.split(",");
  if(items.length>2){items.length=2;};
  return items.join(",");
 };
 var kdeMatrix;
 function findKDEMatrix(bivariate)
 {
  var i,j;
  if(bivariate)
  {
   kdeMatrix=setUpMatrix(bivariateList());
  }
  else
  {
   kdeMatrix=setUpMatrix(plotInfo.pcaVariables);
  };
  kdeMatrix.colTitle="KDE probability distributions ("+kdeMatrix.variables+")";
  kdeMatrix.rowTitle="Datasets";
  for(i=0;i<kdeMatrix.matrixData.length;i++)
  {
   for(j=0;j<kdeMatrix.matrixData.length;j++)
   {
    kdeMatrix.m[i][j]=
     plotCalc[kdeMatrix.matrixData[j]].fullKDE.kde_rel_prob(
     	plotCalc[kdeMatrix.matrixData[i]].fullKDE.data);
   };
  };
  document.getElementById("kdeMatrixArea").removeChild( document.getElementById("kdeMatrixArea").firstChild);
  document.getElementById("kdeMatrixArea").appendChild(kdeMatrix.display(5,'object'));
 };
 function exportKDEMatrix()
 {
  filecontent=kdeMatrix.output(5,true);
  filename="Untitled.csv";
  saveFileAs("csv");
  showArea('fileDialog');
 };
 var ellipsoidMatrix;
 function findEllipsoidMatrix(bivariate)
 {
  var i,j;
  if(bivariate)
  {
   ellipsoidMatrix=setUpMatrix(bivariateList());
  }
  else
  {
   ellipsoidMatrix=setUpMatrix(plotInfo.pcaVariables);
  };
  ellipsoidMatrix.colTitle="Multivatiate normal probability distributions ("+ellipsoidMatrix.variables+")";
  ellipsoidMatrix.rowTitle="Datasets";
  for(i=0;i<ellipsoidMatrix.matrixData.length;i++)
  {
   for(j=0;j<ellipsoidMatrix.matrixData.length;j++)
   {
    ellipsoidMatrix.m[i][j]=
     plotCalc[ellipsoidMatrix.matrixData[j]].fullKDE.ellipsoid_rel_prob(
     	plotCalc[ellipsoidMatrix.matrixData[i]].fullKDE.data);
   };
  };
  document.getElementById("ellipsoidMatrixArea").removeChild( document.getElementById("ellipsoidMatrixArea").firstChild);
  document.getElementById("ellipsoidMatrixArea").appendChild(ellipsoidMatrix.display(5,'object'));
 };
 function exportEllipsoidMatrix()
 {
  filecontent=ellipsoidMatrix.output(5,true);
  filename="Untitled.csv";
  saveFileAs("csv");
  showArea('fileDialog');
};
 
 function playerRewind()
 {
  editOptions.window.playerRewind();
 };
 function playerPause()
 {
  editOptions.window.playerPause();
 };
 function playerNudgeBack()
 {
  editOptions.window.playerNudgeBack();
 };
 function playerNudgeForward()
 {
  editOptions.window.playerNudgeForward();
 };
 function playerBack()
 {
  editOptions.window.playerBack();
 };
 function playerPlay()
 {
  editOptions.window.playerPlay();
 };
 function playerFastForward()
 {
  editOptions.window.playerFastForward();
 };
 function playerWeight(id,multi)
 {
  return editOptions.window.playerWeight(id,multi);
 };
 var showingPlace=false;
 function playerSetPlace(val)
 {
  if(showingPlace){return;};
  editOptions.window.playerSetPlace(val);
 };
 function playerShowPlace(val,displayVal)
 {
  showingPlace=true;
  document.getElementById('playerBar').style.left=(343+val*3)+"px";
  if(displayVal)
  {
   document.getElementById('playerPlace').value=displayVal;
  };
  showingPlace=false;
 };
 var move_p_switch=false;
 function move_p(event)
 {
  if (event.preventDefault){event.preventDefault();};
  switch(event.type)
  {
  case "mousedown": move_p_switch=true;break;
  case "mouseout": move_p_switch=false;return;
  case "mouseup": move_p_switch=false;break;
  case "mousemove":if(!move_p_switch){return;};break;
  };
  playerSetPlace(Math.round((event.clientX-470)/3));
 };
