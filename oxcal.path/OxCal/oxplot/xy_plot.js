   var firstTime=true;
   var plotNo;
   var mover={active:false,frm_clientX:0,frm_clientY:0,
 	  to_clientX:0,to_clientY:0,tmr:false,relax:false,xmoved:false,ymoved:false,graph:-1};

var   canvas=new Object;
   canvas.scale=1.0; // zoom scaling
   canvas.scaleFont=0.8; // font scaling
   canvas.scaleLine=1.3; // line scaling
   canvas.pxPerCm=35; // px per cm
   canvas.frameWidth=18.0; // cm
   canvas.frameHeight=13.5; // cm

var   plotOptions=new Object;
   plotOptions.multiPlot=0;
   plotOptions.mapPlot="svg";
   plotOptions.background="rgb(255,255,255)";
   plotOptions.plotBackground="rgba(255,255,255,0)";
   plotOptions.plotPosX=3.0; // cm
   plotOptions.plotPosY=1.5;  // cm
   plotOptions.plotPosYOffs=0;  // cm
   plotOptions.plotWidth=11.5; // cm
   plotOptions.plotHeight=11.5; // cm
   plotOptions.showKey=0;
   plotOptions.keyTitle="Key";
   plotOptions.BandW=false;

var   plotInfo=new Object;
   plotInfo.minx=0;
   plotInfo.maxx=1;
   plotInfo.miny=-6;
   plotInfo.maxy=6;
   plotInfo.xlabel="Proportion";
   plotInfo.ylabel="Standard Deviations";
   plotInfo.title="Known Age";
   plotInfo.mapPlot=false;
   plotInfo.hideRectangle=false;
   plotInfo.xtop=false;
   plotInfo.yright=false;
//   plotInfo.key=0;
//   plotInfo.showKey=0;
      
var   plotColor=new Object;
   plotColor.showKey=false;
   plotColor.z_calc="";
   plotColor.zlabel="";
   plotColor.minz=0;
   plotColor.maxz=100
   plotColor.autoz=0;
   plotColor.min_col="rgb(0,0,255)";
   plotColor.max_col="rgb(255,0,0)";

var   plotData=new Array;
   plotData[0]=new Object;
   plotData[0].name="theory";
   plotData[0].line="solid";
   plotData[0].marker="";
   
var   plotCalc=new Array;
   plotCalc[0]=new Object;
   plotCalc[0].x=[0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.4, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5, 0.51, 0.52, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59, 0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 0.68, 0.69, 0.7, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.8, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99];
   plotCalc[0].y=[-2.33, -2.05, -1.88, -1.75, -1.64, -1.55, -1.48, -1.41, -1.34, -1.28, -1.23, -1.17, -1.13, -1.08, -1.04, -0.99, -0.95, -0.92, -0.88, -0.84, -0.81, -0.77, -0.74, -0.71, -0.67, -0.64, -0.61, -0.58, -0.55, -0.52, -0.50, -0.47, -0.44, -0.41, -0.39, -0.36, -0.33, -0.31, -0.28, -0.25, -0.23, -0.20, -0.18, -0.15, -0.13, -0.10, -0.08, -0.05, -0.03, 0.00, 0.03, 0.05, 0.08, 0.10, 0.13, 0.15, 0.18, 0.20, 0.23, 0.25, 0.28, 0.31, 0.33, 0.36, 0.39, 0.41, 0.44, 0.47, 0.50, 0.52, 0.55, 0.58, 0.61, 0.64, 0.67, 0.71, 0.74, 0.77, 0.81, 0.84, 0.88, 0.92, 0.95, 0.99, 1.04, 1.08, 1.13, 1.17, 1.23, 1.28, 1.34, 1.41, 1.48, 1.55, 1.64, 1.75, 1.88, 2.05, 2.33];
   


  function tidyText(str)
  {
   return str.replace(/\x2b\x2f\x2d/g,String.fromCharCode(0x00b1));
  };

  function hsvToRgb(h,s,v,array_return)
  {
   var r,g,b;
   if((0<=h) && (h<=60))
   {
    r=255;
    g=(255*h/60);
    b=0;
   };
   if((60<h) && (h<=120))
   {
    r=(255-255*(h-60)/60);
    g=255;
    b=0;
   };
   if((120<h) && (h<=180))
   {
    r=0;
    g=255;
    b=(255*(h-120)/60);
   };
   if((180<h) && (h<=240))
   {
    r=0;
    g=(255-255*(h-180)/60);
    b=255;
   };
   if((240<h) && (h<=300))
   {
    r=(255*(h-240)/60);
    g=0;
    b=255;
   };
   if((300<h) && (h<=360))
   {
    r=255;
    g=0;
    b=(255-255*(h-300)/60);
   }; 
   r=255-(s*(255-r)/100);
   g=255-(s*(255-g)/100);
   b=255-(s*(255-b)/100);
   r=Math.round(v*r/100);
   g=Math.round(v*g/100);
   b=Math.round(v*b/100);
   if(array_return)
   {
    return [h,s,v];
   }
   else
   {
    return "rgb("+r+","+g+","+b+")";
   };
  };
  
  function rgbToHsv(r,g,b,array_return)
  {
    var h,s,v,min,max;
    max=0;min=255;
    if(r<min){min=r;};if(r>max){max=r;};
    if(g<min){min=g;};if(g>max){max=g;};
    if(b<min){min=b;};if(b>max){max=b;};
    if(max==min)
    {
     h=0;
     s=0;
     v=100*(max/255);
    }
    else
    {
     r=Math.round(255*((r-min)/(max-min)));
     g=Math.round(255*((g-min)/(max-min)));
     b=Math.round(255*((b-min)/(max-min)));
     if((b==0) && (r==255))
     {
      h=Math.round(60*g/255);
     };
     if((b==0) && (g==255))
     {
      h=Math.round(120-60*r/255);
     };
     if((r==0) && (g==255))
     {
      h=Math.round(120+60*b/255);
     };
     if((r==0) && (b==255))
     {
      h=Math.round(240-60*g/255);
     };
     if((g==0) && (b==255))
     {
      h=Math.round(240+60*r/255);
     };
     if((g==0) && (r==255))
     {
      h=Math.round(360-60*b/255);
     };
     if(h==360){h=0;};
     v=100*(max/255);
     s=100*((max-min)/max);
    };
    if(array_return)
    {
     return [h,s,v];
    }
    else
    {
     return "hsv("+h+","+s+","+v+")";
    };
  };
  
  function hexToRgb(hex) {
    // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF")
    var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
    hex = hex.replace(shorthandRegex, function(m, r, g, b) {
        return r + r + g + g + b + b;
    });

    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? 
    	[parseInt(result[1], 16),parseInt(result[2], 16),parseInt(result[3], 16)] : 
    	false;
  };
  
  function txtRgbToHsv(txt,array_return)
  {
   function hexToRgb(hex) {
    // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF")
     var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
     hex = hex.replace(shorthandRegex, function(m, r, g, b) {
        return r + r + g + g + b + b;
     });

     var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
     return result ? 
    	[parseInt(result[1], 16),parseInt(result[2], 16),parseInt(result[3], 16)] : 
    	false;
   };
   var ar,i;
   if(txt.indexOf('#')==0)
   {
    ar=hexToRgb(txt);
   }
   else
   {
    ar=txt.split("(");
    if(ar.length!=2){return false;};
    ar=ar[1].replace(")","").split(",");
    if(ar.length<3){return false;};
    for(i=0;i<ar;i++){ar[i]=Number(ar[i]);};
   };
   return rgbToHsv(ar[0],ar[1],ar[2],array_return);
  };

  function colorValue(z)
  {
  	var mn,mx,cx,cn,p,q;
  	mn=plotColor.minz;
  	mx=plotColor.maxz;
  	cn=plotColor.min_col_hsv;
  	cx=plotColor.max_col_hsv;
  	p=(mx-z)/(mx-mn);
  	if(isNaN(z)){return "rgb(127,127,127)";};
	if(mx>mn)
  	{
  	 if(z>mx){return hsvToRgb(cx[0],cx[1]/4,cx[2]);};
  	 if(z<mn){return hsvToRgb(cn[0],cn[1]/2,cn[2]/2);};
  	}
  	else
  	{
  	 if(z>mn){return hsvToRgb(cn[0],cn[1]/2,cn[2]/2);};
  	 if(z<mx){return hsvToRgb(cx[0],cx[1]/4,cx[2]);};
  	};
  	p=(z-mn)/(mx-mn);
  	q=1-p;
  	return hsvToRgb(q*cn[0]+p*cx[0],q*cn[1]+p*cx[1],q*cn[2]+p*cx[2]);
  };

  function checkId(dt,dc,i,obj,dz)  // dz sets sd away from mean
  {
   var a,cl;
   if(dt.include_id && dc.index && dc.index.length && (obj.tagName!='text') && (typeof(dz)=='undefined'))
   {
    if(dt.data[dc.index[i]].id)
    {
     if(plotNo)
     {
      obj.id=dt.data[dc.index[i]].id+"_"+plotNo;
     }
     else
     {
      obj.id=dt.data[dc.index[i]].id;
     };
    };
   };
   if(dt.include_color && dc.index && dc.index.length)
   {
    if(dt.data[dc.index[i]].color)
    {
     obj.setAttributeNS(null,"style",svgColor(dt.data[dc.index[i]].color,"stroke")
       +";"+svgColor(dt.data[dc.index[i]].color,"fill",0.1));
    };
   };
   if(dt.marker=="image" && dc.index && dc.index.length)
   {
    if(dt.data[dc.index[i]].src)
    {
     obj.setAttributeNS(xlinkNS,"href",dt.data[dc.index[i]].src);
    };
   };
   if(dt.include_url && dc.index && dc.index.length)
   {
    if(dt.data[dc.index[i]].url)
    {
     a=document.createElementNS("http://www.w3.org/2000/svg","a");
     a.setAttributeNS("http://www.w3.org/1999/xlink","href",dt.data[dc.index[i]].url);
     a.setAttributeNS(null,"target","_blank");
     a.appendChild(obj);
     obj=a;
    };
   };
   if(!dt.include_label && dc.index && dc.index.length)
   {
    if(dt.data[dc.index[i]].label)
    {
     a=document.createElementNS("http://www.w3.org/2000/svg","title");
     a.appendChild(document.createTextNode(dt.data[dc.index[i]].label));
     obj.appendChild(a);
    };
   };
   if(dc.z && (dc.z.length>i) && (obj.tagName!='text'))
   {
     if(dc.dz && (dc.dz.length>i) && dz)
     {
      if(plotColor.minz>plotColor.maxz){dz=-dz;};
      cl=colorValue(dc.z[i]+dz*dc.dz[i]);
      obj.setAttributeNS(null,"style",svgColor(cl,"stroke")
       +";"+svgColor(cl,"fill")+";fill-opacity:1.0");    
     }
     else
     {
      cl=colorValue(dc.z[i]);
      obj.setAttributeNS(null,"style",svgColor(cl,"stroke")
       +";"+svgColor(cl,"fill"));    
     };
   };
   return obj;
  };
  
  ViewPort.prototype.error_lines=function(dt,dc,typ,data_obj)
  {
   var i,main_obj,xs1,ys1,xs2,ys2,error_obj,cnt,cntNZ;
   main_obj=this.group();
   error_obj=this.group();
   xs1="";xs2="";ys1="";ys2="";cnt=0;cntNZ=0;
   for(i=0;i<dc[typ].length;i++)
   {
    if(isNaN(dc[typ][i])||isNaN(dc.x[i])||isNaN(dc.y[i]))
    {
     if(xs1)
     {
      obj=this.polyline("",xs1.split(","),ys1.split(","),dt.line);
      main_obj.appendChild(obj);
      obj=this.polyline("",xs2.split(","),ys2.split(","),dt.line);
      main_obj.appendChild(obj);
     };
     xs1="";xs2="";ys1="";ys2="";cnt=0;
    }
    else
    {
     if(xs1){xs1=xs1+", ";xs2=", "+xs2;ys1=ys1+", ";ys2=", "+ys2;};
     switch(typ)
     {
     case "dx":
      xs1=xs1+(dc.x[i]-dc.dx[i]);xs2=(dc.x[i]+dc.dx[i])+xs2;
      ys1=ys1+(dc.y[i]);ys2=(dc.y[i])+ys2;
      if(dc.dx[i]!=0){cntNZ++;};
      break;
     case "dy":
      xs1=xs1+(dc.x[i]);xs2=(dc.x[i])+xs2;
      ys1=ys1+(dc.y[i]-dc.dy[i]);ys2=(dc.y[i]+dc.dy[i])+ys2;
      if(dc.dy[i]!=0){cntNZ++;};
      break;
     };
     cnt++;
     if((cnt>1)&&(cntNZ>1))
     {
      obj=this.polygon("",(xs1+","+xs2).split(",").slice(cnt-2,cnt+2),
       (ys1+","+ys2).split(",").slice(cnt-2,cnt+2));
      error_obj.appendChild(obj);
     };
    };
   };
   if(xs1 && (cntNZ>1))
   {
    obj=this.polyline("",xs1.split(","),ys1.split(","),dt.line);
    main_obj.appendChild(obj);
    obj=this.polyline("",xs2.split(","),ys2.split(","),dt.line);
    main_obj.appendChild(obj);
   };
   error_obj.setAttributeNS(null,"style","stroke:none;"+svgColor(dt.markerFill,"fill"));
   main_obj.setAttributeNS(null,"style",svgColor(dt.markerColor,"stroke")+";fill:none");
   data_obj.appendChild(error_obj);
   data_obj.appendChild(main_obj);
  };

  ViewPort.prototype.xy_data=function(dt,dc)
  {
   var i,j,data_obj,main_obj,obj,xa,ya,x,y,anc,mk,ms=3.75,g;
   var dzv=[-0.9,0,0.65,1.3,2.1]; 
   data_obj=this.group();
   if(dt.id)
   {
    data_obj.setAttributeNS(null,"id",dt.id);
   };
   if(dt.line)
   {
    main_obj=this.group();
    if(!dc){return;};
    if(!dc.x){return;};
    for(i=0;i<dc.x.length;i++)
    {
     if(isNaN(dc.x[i])||isNaN(dc.y[i])){dc.x[i]=NaN;dc.y[i]=NaN;};
    };
    xa=dc.x.join(",").split(",NaN,");
    ya=dc.y.join(",").split(",NaN,");
    for(i=0;i<xa.length;i++)
    {
     xa[i]=xa[i].replace("NaN,","").replace(",NaN","");
     ya[i]=ya[i].replace("NaN,","").replace(",NaN","");
     obj=this.polyline("",xa[i].split(","),ya[i].split(","),dt.line);
     main_obj.appendChild(obj);
    };
    main_obj.setAttributeNS(null,"style","fill:none;"+svgColor(dt.lineColor,"stroke"));
    data_obj.appendChild(main_obj);
    if(!mover.active)
    {
     if(!dt.marker && dc.dx && dc.dx.length)
     {
      this.error_lines(dt,dc,"dx",data_obj);
     };
     if(!dt.marker && dc.dy && dc.dy.length)
     {
      this.error_lines(dt,dc,"dy",data_obj);
     };
    };
   };
   if(dt.contour)
   {
    for(i=0;i<dc.xc.length;i++)
    {
     if(isNaN(dc.xc[i])||isNaN(dc.yc[i])){dc.xc[i]=NaN;dc.yc[i]=NaN;};
    };
    xa=dc.xc.join(",").split(",NaN,");
    ya=dc.yc.join(",").split(",NaN,");
    main_obj=this.group();
    for(i=0;i<xa.length;i++)
    {
     xa[i]=xa[i].replace("NaN,","").replace(",NaN","");
     ya[i]=ya[i].replace("NaN,","").replace(",NaN","");
     obj=this.polygon("",xa[i].split(","),ya[i].split(","));
     obj.setAttributeNS(null,"style",svgColor(dt.markerColor,"stroke")
       +";"+svgColor(dt.markerFill,"fill",0.1));
     main_obj.appendChild(obj);
    };
    data_obj.appendChild(main_obj);
   };
   main_obj=this.group();
   if(!mover.active)
   {
   if(dc.dx && dc.dx.length)
   {
    if(dc.dy && dc.dy.length)
    {
     for(i=0;i<dc.x.length;i++)
     {
      if(isNaN(dc.x[i])||isNaN(dc.y[i])||isNaN(dc.dx[i])||isNaN(dc.dy[i])){continue;};
      switch(dt.marker)
      {
      case "image":
       if(dc.y[i] || dc.dy[i])
       {
        obj=this.image(dc.x[i]-dc.dx[i],dc.y[i]-dc.dy[i],2*dc.dx[i],2*dc.dy[i]);
        obj=checkId(dt,dc,i,obj);
        main_obj.appendChild(obj);
       };
       break;
      case "rectangle":
       if(dc.y[i] || dc.dy[i])
       {
        obj=this.rectangle(dc.x[i]-dc.dx[i],dc.y[i]-dc.dy[i],2*dc.dx[i],2*dc.dy[i]);
        obj=checkId(dt,dc,i,obj);
        main_obj.appendChild(obj);
       };
       break;
      case "histogram":
       if(dc.y[i])
       {
        if(dc.dx[i])
        {
         obj=this.rectangle(dc.x[i]-dc.dx[i],0,2*dc.dx[i],dc.y[i]);
         obj=checkId(dt,dc,i,obj);
        }
        else
        {
         obj=this.line(dc.x[i],0,dc.x[i],dc.y[i]);
         obj=checkId(dt,dc,i,obj);
        };
       }
       else
       {
        obj=this.line(dc.x[i]-dc.dx[i],0,dc.x[i]+dc.dx[i],0);
        obj=checkId(dt,dc,i,obj);
       };
       main_obj.appendChild(obj);
       if(dc.dx[i] || dc.dy[i])
       {
        obj=this.errorbars(dc.x[i],dc.y[i],0,dc.dy[i],0);
        main_obj.appendChild(obj);
       };
       break;
      default:
       if((dc.dx[i] || dc.dy[i])&&(dt.markerColor!="rgba(0,0,0,0)"))
       {
        obj=this.errorbars(dc.x[i],dc.y[i],dc.dx[i],dc.dy[i],0);
        main_obj.appendChild(obj);
       };
      };
     };
    }
    else
    {
     for(i=0;i<dc.x.length;i++)
     {
      if(isNaN(dc.x[i])||isNaN(dc.y[i])||isNaN(dc.dx[i])){continue;};
      if(dt.marker=="histogram")
      {
       if(dc.y[i])
       {
        if(dc.dx[i])
        {
         obj=this.rectangle(dc.x[i]-dc.dx[i],0,2*dc.dx[i],dc.y[i]);
        }
        else
        {
         obj=this.line(dc.x[i],0,dc.x[i],dc.y[i]);
        };
       }
       else
       {
        obj=this.line(dc.x[i]-dc.dx[i],0,dc.x[i]+dc.dx[i],0);
       };
       obj=checkId(dt,dc,i,obj);
       main_obj.appendChild(obj);
      }
      else
      {
       if(dc.dx[i] &&(dt.markerColor!="rgba(0,0,0,0)"))
       {
        obj=this.errorbars(dc.x[i],dc.y[i],dc.dx[i],0,0);
        main_obj.appendChild(obj);
       };
      };
     };
    };
   }
   else
   {
    if(dc.dy && dc.dy.length)
    {
     for(i=0;i<dc.x.length;i++)
     {
      if(isNaN(dc.x[i])||isNaN(dc.y[i])||isNaN(dc.dy[i])){continue;};
      if(dt.marker=="histogram")
      {
       if(dc.x[i])
       {
        if(dc.dy[i])
        {
         obj=this.rectangle(0,dc.y[i]-dc.dy[i],dc.x[i],2*dc.dy[i]);
        }
        else
        {
         obj=this.line(0,dc.y[i],dc.x[i],dc.y[i]);
        };
       }
       else
       {
        obj=this.line(0,dc.y[i]-dc.dy[i],0,dc.y[i]+dc.dy[i]);
       };
       obj=checkId(dt,dc,i,obj);
       main_obj.appendChild(obj);
      }
      else
      {
       if(dc.dy[i] && (dt.markerColor!="rgba(0,0,0,0)"))
       {
        obj=this.errorbars(dc.x[i],dc.y[i],0,dc.dy[i],0);
        main_obj.appendChild(obj);
       };
      };
     };
    };
   };
   };
   if(dt.marker || (dc.label && dc.label.length))
   {
    for(i=0;i<dc.x.length;i++)
    {
     if(isNaN(dc.x[i]) || isNaN(dc.y[i])){continue;};
     switch(dt.marker)
     {
     case "image":
     case "rectangle":
      break;
     case "label":
      obj=this.text(dc.x[i],dc.y[i],dt.data[dc.index[i]].label,10,"middle",-0.5,svgColor(dt.markerFill));
      obj=checkId(dt,dc,i,obj);
      main_obj.appendChild(obj);
      break;
     case "histogram":
      if(!(dc.dx && dc.dx.length) && !(dc.dy && dc.dy.length))
      {
       obj=this.line(dc.x[i],0,dc.x[i],dc.y[i]);
       obj=checkId(dt,dc,i,obj);
       main_obj.appendChild(obj);
      };
      break;
     default:
      if(dt.include_label && dt.data[dc.index[i]].label)
      {
       obj=this.text(dc.x[i], dc.y[i],dt.data[dc.index[i]].label, 10,"start",0.75,";fill-opacity:1.0");
       obj=checkId(dt,dc,i,obj);
       main_obj.appendChild(obj);
      };
      mk=dt.marker;
      if(dc.index && dc.index.length && dt.include_marker)
      {
       if(dt.data[dc.index[i]].marker){mk=dt.data[dc.index[i]].marker;};
       if(dt.data[dc.index[i]].markerSize){ms=dt.data[dc.index[i]].markerSize;};
      };
      if(dc.dz && (dc.dz.length>i))
      {
       g=this.createElement('g');
       checkId(dt,dc,i,g);
       for(j=0;j<dzv.length;j++)
       {
        obj=this.symbol(dc.x[i],dc.y[i],ms*(dzv.length-j)/dzv.length,mk);
        obj=checkId(dt,dc,i,obj,dzv[j]);
        g.appendChild(obj);    
       };
       main_obj.appendChild(g);    
      }
      else
      {
       obj=this.symbol(dc.x[i],dc.y[i],ms,mk);
       obj=checkId(dt,dc,i,obj);
       main_obj.appendChild(obj);
      };
      break;
     };
    };
    main_obj.setAttributeNS(null,"style",svgColor(dt.markerColor,"stroke")+";"+svgColor(dt.markerFill,"fill"));
   }
   else
   {
    main_obj.setAttributeNS(null,"style",svgColor(dt.markerColor,"stroke"));
   };
   if(dt.marker || dc.dy || dc.dx)
   {
    data_obj.appendChild(main_obj);
   };
   if(dt.url)
   {
    anc=document.createElementNS("http://www.w3.org/2000/svg","a");
    anc.setAttributeNS("http://www.w3.org/1999/xlink","href",dt.url);
    anc.setAttributeNS(null,"target","_blank");
    anc.appendChild(data_obj);
    this.appendChild(anc);
   }
   else
   {
    this.appendChild(data_obj);
   };
  };

  function checkMin(dc,type,m)
  {
   var i,v;
   if(!dc[type] || (dc[type].length==0)){return m;};
   if(dc["d"+type] && dc["d"+type].length)
   {
    for(i=0;i<dc[type].length;i++)
    {
     if((m>dc[type][i]-dc["d"+type][i])||isNaN(m)||(m=="auto"))
     {
      m=dc[type][i]-dc["d"+type][i];
     };
    };
   }
   else
   {
    for(i=0;i<dc[type].length;i++)
    {
     if((m>dc[type][i])||isNaN(m)||(m=="auto"))
     {
      m=dc[type][i];
     };
    };
   };
   return m;
  };

  function checkMax(dc,type,m)
  {
   var i,v;
   if(!dc[type] || (dc[type].length==0)){return m;};
   if(dc["d"+type] && dc["d"+type].length)
   {
    for(i=0;i<dc[type].length;i++)
    {
     if((m<dc[type][i]+dc["d"+type][i])||isNaN(m)||(m=="auto"))
     {
      m=dc[type][i]+dc["d"+type][i];
     };
    };
   }
   else
   {
    for(i=0;i<dc[type].length;i++)
    {
     if((m<dc[type][i])||isNaN(m)||(m=="auto"))
     {
      m=dc[type][i];
     };
    };
   };
   return m;
  };
  
  ViewPort.prototype.add_svg_map=function(pa,pi,x,y,w,h)
  {
   var sym;
   sym=pa.createElement("use");
   sym.setAttributeNS(xlinkNS,"xlink:href","#WorldMap");
   sym.setAttributeNS(null,"width",w);
   sym.setAttributeNS(null,"height",h);
   sym.setAttributeNS(null,"x",x);
   sym.setAttributeNS(null,"y",y);
   sym.setAttributeNS(null,"class","greyland");
   if(plotOptions.mapForeground)
   {
    sym.setAttributeNS(null,"style",svgColor(plotOptions.mapForeground,"fill")+";"+
    	svgColor(plotOptions.mapForeground,"stroke"));
   };
   pa.appendChild(sym);
  };
  
  ViewPort.prototype.xy_map=function(pa,pi)
  {
   var str,w,h,t,z,res,rx,ry,zx,map_sc,cx,cy;
   if(!plotOptions.mapPlot){plotOptions.mapPlot="svg";};
   if(pi.maxy>85){pi.maxy=85;};
   if(pi.maxy<-84){pi.maxy=-84;};
   if(pi.miny>84){pi.miny=84;};
   if(pi.miny<-85){pi.miny=-85;};
   if(pi.reversex){t=pi.minx;pi.minx=pi.maxx;pi.maxx=t;pi.reversex=false;};
   if(pi.reversey){t=pi.minx;pi.minx=pi.maxx;pi.maxx=t;pi.reversex=false;};
   w=Math.round(plotOptions.plotWidth*canvas.pxPerCm*scale);
   h=Math.round(plotOptions.plotHeight*canvas.pxPerCm*scale);
   cx=(pi.minx+pi.maxx)/2;
   cy=inv_mercator((mercator(pi.miny)+mercator(pi.maxy))/2);
   cx=cx-Math.round(cx/360)*360;
   rx=Math.abs(pi.maxx-pi.minx);
   ry=Math.abs(mercator(pi.maxy)-mercator(pi.miny));
   pi.minx=cx-rx/2;
   pi.maxx=cx+rx/2;
   if((rx/w > ry/h)&&(pi.autox))
   {
    ry=rx*h/w;
    pi.miny=inv_mercator(mercator(cy)-ry/2);
    pi.maxy=inv_mercator(mercator(cy)+ry/2);
   }
   else
   {
    if(rx/w != ry/h)
    {
     rx=ry*w/h;
     pi.minx=cx-rx/2;
     pi.maxx=cx+rx/2;
    };
   };
   if(plotOptions.mapPlot=='svg')
   {
    w=(pa.ux(1)-pa.ux(0));
    h=(pa.uy(0));
    z=mercator(pi.maxy);
    this.add_svg_map(pa,pi,-w*(pi.minx+180)/rx,w*(z-180)/rx,w*360/rx,w*360/rx);
    if(pi.maxx>180)
    {
     this.add_svg_map(pa,pi,w*360/rx-w*(pi.minx+180)/rx,w*(z-180)/rx,w*360/rx,w*360/rx);
    };
    if(pi.minx<-180)
    {
     this.add_svg_map(pa,pi,-w*(pi.minx+180)/rx-w*360/rx,w*(z-180)/rx,w*360/rx,w*360/rx);
    };
   }
   else
   {
    z=0;
    zx=360/256;
    while((zx>=2*rx/w)&&(z<16))
    {
     zx/=2;
     z+=1;
    };
    map_sc=1;
    while(Math.round(plotOptions.plotWidth*canvas.pxPerCm*scale/map_sc)>640 ||
       Math.round(plotOptions.plotHeight*canvas.pxPerCm*scale/map_sc)>640)
    {
     z-=1;map_sc*=2;
    };
    str="http://maps.googleapis.com/maps/api/staticmap";
    str+="?size="+Math.round(plotOptions.plotWidth*canvas.pxPerCm*scale/map_sc)+"x"
      +Math.round(plotOptions.plotHeight*canvas.pxPerCm*scale/map_sc);
    str+="&scale=2";
    str+="&format=PNG";
    str+="&maptype="+plotOptions.mapPlot;
    str+="&zoom="+z;
    str+="&center="+roundFactor(cy,1000000)+","+roundFactor(cx,1000000);
    str+="&sensor=false";
    pa.appendChild(pa.image((1-w*zx/rx)/2,(1-w*zx/rx)/2,w*zx/rx,w*zx/rx,str));
   };
 };
 
  ViewPort.prototype.xy_plot=function(pi,pcol,pf,pd,pc,frm)
  {
    var plotarea,i,r,t;
    if(typeof(pf.plotPosYOffs)=='undefined'){pf.plotPosYOffs=0;};
    //check axes
    if(pi.autox)
    {
     pi.minx="auto";pi.maxx="auto";
     for(i=0;i<pc.length;i++){pi.minx=checkMin(pc[i],"x",pi.minx);};
     for(i=0;i<pc.length;i++){pi.maxx=checkMax(pc[i],"x",pi.maxx);};
     if(!isNaN(r=pi.maxx-pi.minx))
     {
      if(pi.minx * (pi.minx-r/10) > 0){pi.minx=pi.minx-r/10;}else{pi.minx=0;};
      if(pi.maxx * (pi.maxx+r/10) > 0){pi.maxx=pi.maxx+r/10;}else{pi.maxx=0;};
     };
     if(pi.autox==2)
     {
      t=pi.minx;
      pi.minx=pi.maxx;
      pi.maxx=t;
     };
    };
    if(pi.autoy)
    {
     pi.miny="auto";pi.maxy="auto";
     for(i=0;i<pc.length;i++){pi.miny=checkMin(pc[i],"y",pi.miny);};
     for(i=0;i<pc.length;i++){pi.maxy=checkMax(pc[i],"y",pi.maxy);};
     if(!isNaN(r=pi.maxy-pi.miny))
     {
      if(pi.miny * (pi.miny-r/10) > 0){pi.miny=pi.miny-r/10;}else{pi.miny=0;};
      if(pi.maxy * (pi.maxy+r/10) > 0){pi.maxy=pi.maxy+r/10;}else{pi.maxy=0;};
     };
     if(pi.autoy==2)
     {
      t=pi.miny;
      pi.miny=pi.maxy;
      pi.maxy=t;
     };
    };
    if(pcol.z_calc && pcol.autoz)
    {
     pcol.minz="auto";pcol.maxz="auto";
     for(i=0;i<pc.length;i++){pcol.minz=checkMin(pc[i],"z",pcol.minz);};
     for(i=0;i<pc.length;i++){pcol.maxz=checkMax(pc[i],"z",pcol.maxz);};
     if(!isNaN(r=pcol.maxz-pcol.minz))
     {
      if(pcol.minz * (pcol.minz-r/10) > 0){pcol.minz=pcol.minz-r/10;}else{pcol.minz=0;};
      if(pcol.maxz * (pcol.maxz+r/10) > 0){pcol.maxz=pcol.maxz+r/10;}else{pcol.maxz=0;};
     };
     if(pcol.autoz==2)
     {
      t=pcol.minz;
      pcol.minz=pcol.maxz;
      pcol.maxz=t;
     };
    };
    if(pi.minx==pi.maxx)
    {
     if(pi.minx=="auto"){pi.minx=0;};
     pi.minx-=0.5;
     pi.maxx=pi.minx+1;
    };
    if((pi.nonlinx=='log')&&(pi.minx<=0))
    {
     pi.minx=1;
     if(pi.maxx<=pi.minx){pi.maxx=10;};
    };
    if(pi.miny==pi.maxy)
    {
     if(pi.miny=="auto"){pi.miny=0;};
     pi.miny-=0.5;
     pi.maxy=pi.miny+1;
    };
    if((pi.nonliny=='log')&&(pi.miny<=0))
    {
     pi.miny=1;
     if(pi.maxy<=pi.miny){pi.maxy=10;};
    };
    if(pcol.minz==pcol.maxz)
    {
     if(pcol.minz=="auto"){pcol.minz=0;};
     pcol.minz-=0.5;
     pcol.maxz=pcol.minz+1;
    };
    
    // define area of plot

	pi.reversex=(pi.maxx<pi.minx);
	pi.reversey=(pi.maxy<pi.miny);

    
	plotarea=this.viewPort("plotarea"+frm,pf.plotPosX,pf.plotPosY+pf.plotPosYOffs,
	 pf.plotWidth,pf.plotHeight);
	plotarea.map(0,0,1,1);
	plotarea.clipPath(plotarea.rectangle(0,0,1,1,""));
	if(pi.mapPlot && (plotOptions.mapPlot!="svg"))
	{
	 this.xy_map(plotarea,pi);
	};
	if(!plotOptions.plotBackground){plotOptions.plotBackground="rgba(255,255,255,0)";};
	plotarea.appendChild(plotarea.styledRectangle(-0.1,-0.1,1.2,1.2,
	  svgColor(plotOptions.plotBackground,"fill")));
	if(pi.mapPlot && (plotOptions.mapPlot=="svg"))
	{
	 this.xy_map(plotarea,pi);
	};
	plotarea.map(pi.minx,pi.miny,pi.maxx-pi.minx,pi.maxy-pi.miny,pi.nonlinx,pi.nonliny);
	for(i=0;i<pd.length;i++)
	{
	 plotarea.xy_data(pd[i],pc[i]);
	};

	// put all of the elements on the plot
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
	this.appendChild(plotarea.element);
	if(!pi.hideRectangle)
	{
	 this.appendChild(this.rectangle(pf.plotPosX,pf.plotPosY+pf.plotPosYOffs,
	   pf.plotWidth,pf.plotHeight,"border"));
	};
	this.xlabel=tidyText(pi.xlabel);
	this.xlabeller=labellerChoice(this.xlabel);
	this.ylabel=tidyText(pi.ylabel);
	this.ylabeller=labellerChoice(this.ylabel);
	if(pf.plotHeight>pf.plotWidth)
	{
	 this.maxXLabel=Math.round(this.maxYLabel*pf.plotWidth/pf.plotHeight);
	 if(this.maxXLabel<3){this.maxXLabel=3;};
	}
	else
	{
	 this.maxYLabel=Math.round(this.maxXLabel*pf.plotHeight/pf.plotWidth);
	 if(this.maxYLabel<3){this.maxYLabel=3;};
	};
  	this.mapRectangle(pf.plotPosX,pf.plotPosY+pf.plotPosYOffs,
  	  pf.plotWidth,pf.plotHeight,
  	  pi.minx,0,pi.maxx-pi.minx,pf.plotHeight,pi.nonlinx,false);
    if(pi.reversex){r=pi.minx;pi.minx=pi.maxx;pi.maxx=r;};
    if(pf.showGrid){this.grid("x",pi.minx,pi.maxx,pf.plotHeight);};
    if(pi.xtop)
    {
	 this.axis("xt",pi.minx,pi.maxx);
    }
    else
    {
	 this.axis("x",pi.minx,pi.maxx);
    };
    if(pi.reversex){r=pi.minx;pi.minx=pi.maxx;pi.maxx=r;};
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
  	this.mapRectangle(pf.plotPosX,pf.plotPosY+pf.plotPosYOffs,
  	  pf.plotWidth,pf.plotHeight,
  	  0,pi.miny,pf.plotWidth,pi.maxy-pi.miny,false,pi.nonliny);
    if(pi.reversey){r=pi.miny;pi.miny=pi.maxy;pi.maxy=r;};
    if(pf.showGrid){this.grid("y",pi.miny,pi.maxy,pf.plotWidth);};
    if(pi.yright)
    {
	 this.axis("yr",pi.miny,pi.maxy);
	}
	else
	{
	 this.axis("y",pi.miny,pi.maxy);
	};
    if(pi.reversey){r=pi.miny;pi.miny=pi.maxy;pi.maxy=r;};
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
  	this.mapRectangle(pf.plotPosX,pf.plotPosY+pf.plotPosYOffs,
  	  pf.plotWidth,pf.plotHeight,
  	  0,0,1,1);
  	t=this.text(0.5,0.98,tidyText(pi.title),12,"middle",-1.25);
  	if(plotNo)
  	{
  	 t.id="title"+plotNo;
  	}
  	else
  	{
  	 t.id="title";
  	};
	this.appendChild(t);
  };
  
  ViewPort.prototype.key=function(pi,pf,pd)
  {
    var keyarea,i,pos,obj;
    var x=[0.25,1.75];
    var y=[0,0];
    
	keyarea=this.viewPort("keyarea",pf.keyPosX,pf.keyPosY+pf.plotPosYOffs,
	 pf.keyWidth,pf.keyHeight);
	keyarea.map(0,-0.5,pf.keyWidth,pf.keyCount+2);
	keyarea.clipPath(keyarea.rectangle(0,-0.5,pf.keyWidth,pf.keyCount+2,""));
	pos=pf.keyCount-0.5;
	for(i=0;i<pd.length;i++)
	{
	 if(!pd[i].name){continue;};
	 if(!pd[i].selected){continue;};
	 if(pd[i].line)
	 {
	  y[0]=y[1]=pos;
      obj=keyarea.polyline("",x,y,pd[i].line);
      obj.setAttributeNS(null,"style","fill:none;"+svgColor(pd[i].lineColor,"stroke"));
      keyarea.appendChild(obj);
	 };
	 if(pd[i].contour)
	 {
	  obj=keyarea.circle(1,pos,6);
      obj.setAttributeNS(null,"style",svgColor(pd[i].markerColor,"stroke")+ ";"+svgColor(pd[i].markerFill,"fill",0.1));
      keyarea.appendChild(obj);
	 };
	 if(pd[i].marker && (pd[i].marker!='label'))
	 {
	  if(pd[i].marker=='histogram')
	  {
	   obj=keyarea.rectangle(0.5,pos-0.25,1,0.5);
	  }
	  else
	  {
       obj=keyarea.symbol(1.0,pos,0,pd[i].marker);
      };
      obj.setAttributeNS(null,"style",svgColor(pd[i].markerColor,"stroke")+ ";"+svgColor(pd[i].markerFill,"fill"));
      keyarea.appendChild(obj);
	 };
	 keyarea.appendChild(keyarea.text(2,pos,tidyText(pd[i].name),10,"start",-0.5));
	 pos--;
	};
	keyarea.appendChild(keyarea.text(pf.keyWidth/2,pf.keyCount+0.5, tidyText(pi.keyTitle),12,"middle",-0.25));

	// put all of the elements on the plot
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
	this.appendChild(keyarea.element);
	this.appendChild(this.rectangle(pf.keyPosX,pf.keyPosY+pf.plotPosYOffs,
	  pf.keyWidth,pf.keyHeight,"border"));
  	this.mapRectangle(pf.keyPosX,pf.keyPosY+pf.plotPosYOffs,
  	  pf.keyWidth,pf.keyHeight,
  	  0,0,1,1);
  };
  
  
  ViewPort.prototype.colScale=function(pi,pf)
  {
    var colarea,r,t,i;
    
	if(isNaN(pi.minz)||isNaN(pi.maxz)){return;};
	pi.reversez=(pi.maxz<pi.minz)
	this.ylabel=tidyText(pi.zlabel);
	this.ylabeller=labellerChoice(this.ylabel);
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
	colarea=this.viewPort("plotareaColorScale",pf.colPosX,pf.colPosY+pf.plotPosYOffs,
	 1,pf.plotHeight);
	colarea.map(0,0,1,1);
	colarea.clipPath(colarea.rectangle(0,0,1,1,""));
	for(i=0;i<256;i++)
	{
	 colarea.appendChild(colarea.styledRectangle(0,i/256,1,1/200,
	  "stroke:none;fill:"+colorValue(pi.minz+(pi.maxz-pi.minz)*i/256)));
	};
	this.appendChild(colarea.element);
	this.appendChild(this.rectangle(pf.colPosX,pf.colPosY+pf.plotPosYOffs,
	 1,pf.plotHeight,"border"));
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
  	this.mapRectangle(pf.colPosX,pf.colPosY+pf.plotPosYOffs,
  	  1,pf.plotHeight,
  	  0,pi.minz,1,pi.maxz-pi.minz);
    if(pi.reversez){r=pi.minz;pi.minz=pi.maxz;pi.maxz=r;};
    if(pf.showGrid){this.grid("y",pi.minz,pi.maxz,pf.plotWidth);};
	this.axis("y",pi.minz,pi.maxz);
    if(pi.reversez){r=pi.minz;pi.minz=pi.maxz;pi.maxz=r;};
    this.map(0,0,this.canvas.frameWidth,this.canvas.frameHeight);
  };
  
  function stripPlot()
  {
   var el;
   el=document.getElementById("mainframe");
   while(el.firstChild)
   {
    el.removeChild(el.firstChild);
   };
  };

  function draw()
  {
    var mainViewport;
    var mainFrame;
    var fontMultiplier,zoomMultiplier,area;
    var col_x,col_y;
    // link plot to SVG document
		
    plotNo=0;
	stripPlot();

    fontMultiplier=1;zoomMultiplier=1;
    if(plotOptions.multiPlot)
    {
     area=plotOptions.columns*(plotOptions.maxrow+1);
     if((area>4)&&(plotOptions.columns>1)&&(plotOptions.maxrow>0))
     {
      zoomMultiplier=2/Math.sqrt(area);
      fontMultiplier=1/Math.sqrt(zoomMultiplier);
     };
    };
    
    canvas.zoomMultiplier=zoomMultiplier;
    mainFrame=attachSVGDocument(canvas.frameWidth,canvas.frameHeight,canvas.scale*zoomMultiplier,canvas.scaleFont*fontMultiplier,canvas.scaleLine,plotOptions.background);
	mainFrame.map(0,0,canvas.frameWidth,canvas.frameHeight);
   
	// set up main viewport
	mainViewport=mainFrame.viewPort("mainviewport",0,0,canvas.frameWidth,canvas.frameHeight);
    mainViewport.map(0,0,canvas.frameWidth,canvas.frameHeight);
    mainViewport.canvas=canvas;
    mainFrame.appendChild(mainViewport.element);
    
    // set up color scale
    plotColor.min_col_hsv=txtRgbToHsv(plotColor.min_col,true);
    plotColor.max_col_hsv=txtRgbToHsv(plotColor.max_col,true);
    
    // put on the plot
    if(plotOptions.multiPlot)
    {
     if(window.parent && window.parent.firstPlot && window.parent.firstPlot(mover.active,mover.graph))
     {
      do
      {
       plotNo++;
	   if(window.opener)
	   {
	    plotCalc=window.opener.plotCalc;
	   };
	   if(window.parent)
	   {
	    plotCalc=window.parent.plotCalc;
	   };
 	   if((mover.active && (mover.graph != parent.multiPos) && (mover.graph != -1)))
	   {
	    continue;
	   };
       if(plotOptions.key)
       {
        mainViewport.map(0,0,canvas.frameWidth,canvas.frameHeight);
        mainViewport.key(plotInfo,plotOptions,plotData);
       }
       else
       {
        mainViewport.map(0,0,canvas.frameWidth,canvas.frameHeight);
        mainViewport.xy_plot(plotInfo,plotColor,plotOptions,plotData,plotCalc,parent.multiPos);
       };
      }
      while(window.parent.nextPlot(mover.active,mover.graph));
     };
    }
    else
    {
     if(!(mover.active && mover.graph=="ColorScale"))
     {
      if(plotOptions.showKey!=2)
      {
       mainViewport.xy_plot(plotInfo,plotColor,plotOptions,plotData,plotCalc,"");
      };
      if(plotOptions.key)
      {
       mainViewport.map(0,0,canvas.frameWidth,canvas.frameHeight);
       mainViewport.key(plotInfo,plotOptions,plotData);
      };
     };
    };
    if(plotColor.showKey)
    {
     mainViewport.colScale(plotColor,plotOptions);
    };
  };
        
	function createPlot()
	{
	 if(window.opener)
	 {
	  canvas=window.opener.canvas;
	  plotInfo=window.opener.plotInfo;
	  plotOptions=window.opener.plotOptions;
	  plotData=window.opener.plotData;
	  plotCalc=window.opener.plotCalc;
	  plotColor=window.opener.plotColor;
	 };
	 if(window.parent)
	 {
	  canvas=window.parent.canvas;
	  plotInfo=window.parent.plotInfo;
	  plotOptions=window.parent.plotOptions;
	  plotData=window.parent.plotData;
	  plotCalc=window.parent.plotCalc;
	  plotColor=window.parent.plotColor;
	 };
	 draw();
	 if(window.parent)
	 {
	  window.parent.plotInfo=plotInfo;
	  if(window.parent.viewSpec)
	  {
	   window.parent.viewSpec.refillContainer(true);
	  };
	  if(window.parent.finishPlot)
	  {
	   window.parent.finishPlot();
	  };
	 };
 	};
 	function move_testx(x,y)
 	{
 	 if(mover.xmoved){return true;};
 	 if(Math.abs(x)>Math.abs(y)){mover.xmoved=true;return true;};
 	 return false;
 	};
 	function move_testy(x,y)
 	{
 	 if(mover.ymoved){return true;};
 	 if(Math.abs(y)>Math.abs(x)){mover.ymoved=true;return true;};
 	 return false;
 	};
 	function do_action()
 	{
 	 var x,y;
 	 var cm=canvas.pxPerCm*canvas.scale*canvas.zoomMultiplier;
     switch(mover.action)
     {
      case "zoom":
       if((mover.frm_clientX>10) && (mover.frm_clientY>10) && 
         (mover.to_clientX>10) && (mover.to_clientY>10))
       {
        parent.zoom((mover.to_clientX/mover.frm_clientX + mover.to_clientY/mover.frm_clientY)/2);
       };
       break;
      case "span":
       x=(mover.frm_clientX-mover.to_clientX)/(plotOptions.plotWidth*cm);
       y=(mover.to_clientY-mover.frm_clientY)/(plotOptions.plotHeight*cm);       
       if((mover.frm_clientX>10) && (mover.frm_clientY>10) && 
         (mover.to_clientX>10) && (mover.to_clientY>10))
       {
        if(mover.graph==-1)
        {
         if(move_testx(x,y)){parent.changeXRange(Math.exp(x));};
         if(move_testy(x,y)){parent.changeYRange(Math.exp(y));};
        }
        else
        {
         if(move_testx(x,y)){parent.singlePlotAction(parent.changeXRangeInf,mover.graph,Math.exp(x));};
         if(move_testy(x,y)){parent.singlePlotAction(parent.changeYRangeInf,mover.graph,Math.exp(y));};
        };
       };
       break;
      case "move":
       x=(mover.frm_clientX-mover.to_clientX)/(plotOptions.plotWidth*cm);
       y=(mover.to_clientY-mover.frm_clientY)/(plotOptions.plotHeight*cm);
       if(mover.graph==-1)
       {
        if(move_testy(x,y)){parent.changeMinY(y);};
        if(move_testx(x,y)){parent.changeMinX(x);};
       }
       else
       {
        if(move_testy(x,y)){parent.singlePlotAction(parent.changeMinYInf,mover.graph,y);};
        if(move_testx(x,y)){parent.singlePlotAction(parent.changeMinXInf,mover.graph,x);};
       };
       break;
     };
     mover.frm_clientX=mover.to_clientX;
     mover.frm_clientY=mover.to_clientY;
 	};
 	function setup_action(event,delay)
 	{
 	 if(mover.tmr){window.clearTimeout(mover.tmr);};
     mover.to_clientX=event.clientX;
     mover.to_clientY=event.clientY;
 	 if(delay)
 	 {
 	  if(!mover.relax)
 	  {
 	   do_action();
 	   mover.relax=true;
 	   window.setTimeout("mover.relax=false;",delay);
 	  };
 	 }
 	 else
 	 {
 	  mover.active=false;
 	  do_action();
 	 };
 	};
 	function findGraph(targ)
 	{
 	 while(((!targ.id)||(targ.id.indexOf("plotarea")!=0))&& targ.parentNode)
 	 {
 	  targ=targ.parentNode;
 	 };
 	 if((!targ.id)||(targ.id.indexOf("plotarea")!=0)){return -1;};
 	 return targ.id.replace("plotarea","");
 	};
 	function move_bd(event)
 	{
 	 var targ;
     if(!event){event=window.event;};//needed for IE
     if(event.button>1){return;};
	 if (event.target){targ = event.target;}
	 else if(e.srcElement){targ = event.srcElement;}; // for IE
	 if(targ.nodeType == 3){targ = targ.parentNode;}; // defeat Safari bug
     if(event.preventDefault){event.preventDefault();};
     switch(event.type)
     {
     case "mousedown": 
     	mover.active=true;
     	mover.frm_clientX=event.clientX;
     	mover.frm_clientY=event.clientY;
     	mover.xmoved=false;mover.ymoved=false;
     	mover.action="move";
     	mover.graph=findGraph(targ);
     	if(mover.graph==""){mover.graph=-1;};
     	if(mover.graph!="ColorScale")
     	{
 	     if(isNaN(mover.graph)){mover.graph=-1;}else{mover.graph=Number(mover.graph);};
 	    };
     	if(event.shiftKey){mover.action="span";};
     	if(event.altKey){mover.action="zoom";};
     	break;
     case "mouseout":
     case "mouseup": 
        if(mover.active){setup_action(event,0);};
        break;
     case "mousemove":
        if(mover.active){setup_action(event,100);};
        break;
     };
 	};
 	function BC_labeller(t,g)
 	{ if(t==0){return "BC/AD";}; if(t<0){return "AD "+(1-Math.round(t));}; 
 	 return Math.round(t); }; 
 	function BCE_labeller(t,g)
 	{ if(t==0){return "BCE/CE";}; if(t<0){return (1-Math.round(t))+" CE";}; 
 	 return Math.round(t); }; 
 	function AD_labeller(t,g)
 	{ if(t==0){return "BC/AD";}; if(t<0){return (1-Math.round(t))+" BC";}; 
 	 return Math.round(t); }; 
 	function CE_labeller(t,g)
 	{ if(t==0){return "BCE/CE";}; if(t<0){return (1-Math.round(t))+" BCE";}; 
 	 return Math.round(t); }; 
    function nullLabeller(a,b){ return ""; };
    function labellerChoice(label)
    {
     var a,b;
     if(label=="nonumbers"){return nullLabeller;};
     a=label.indexOf("(");b=label.indexOf(")");
     if((a>-1)&&(b>a+1))
     {
      switch(label.slice(a+1,b))
      {
      case "BC": return BC_labeller;
      case "BCE": return BCE_labeller;
      case "AD": return AD_labeller;
      case "CE": return CE_labeller;
      };
     };
     return ViewPort.prototype.xlabeller;
    };
 
