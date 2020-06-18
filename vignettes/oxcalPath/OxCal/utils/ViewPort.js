// ViewPort.js
// General Plotting routines
// =======================
  var xlinkNS = "http://www.w3.org/1999/xlink";
  
  function ViewPortDoc()
  {
   return document;
  };
  function roundFactor(num,fact)
  {
   return Math.round(num*fact)/fact;
  };
  function svgColor(col,typ,opac)
  {
   var arr,str="";
   if(typ){str=typ+":";};
   if((col.indexOf("(")!=-1)&&(col.indexOf(")")!=-1))
   {
    arr=col.slice(col.indexOf("(")+1,col.indexOf(")")).split(",");
    if(opac){arr[3]=opac;};
    if(arr.length>3)
    {
     str+="rgb("+arr[0]+","+arr[1]+","+arr[2]+")";
     switch(typ)
     {
     case "stroke":case "fill":
      str+=";"+typ+"-opacity:"+arr[3];
      break;
     };
     return str;
    };
   }
   else
   {
    if(opac)
    {
     str+=col;
     switch(typ)
     {
     case "stroke":case "fill":
      str+=";"+typ+"-opacity:"+opac;
      break;
     };
     return str;
    };
   };
   str+=col;
   return str;
  };
  function mercator(lat)
  {
   lat=Math.PI*lat/180;
   return 180*Math.log(1/Math.cos(lat)+Math.tan(lat))/Math.PI;
  };
  function inv_mercator(lat)
  {
   return 360*Math.atan(Math.exp(Math.PI*lat/180))/Math.PI-90;
  };
  var scale,font_scale,line_scale;
  function attachSVGDocument(w,h,s,font_stretch,line_stretch,background)
  {
   var mainFrame;
   var el;
   var pt_scale;
   if(!font_stretch){font_stretch=0.8;};
   if(!line_stretch){line_stretch=1.0;};
   if(!background){background="white";};
   scale=s;
   font_scale=scale*font_stretch;
   line_scale=line_stretch*font_scale;
   pt_scale=s*28.3;
   el=ViewPortDoc().getElementById("top");
   el.setAttributeNS(null,"width",(w*pt_scale).toFixed(3)+"pt");
   el.setAttributeNS(null,"height",(h*pt_scale).toFixed(3)+"pt");
   el.setAttributeNS(null,"viewBox","0 0 " + (w*pt_scale).toFixed(3) + " " + (h*pt_scale).toFixed(3));

   el=ViewPortDoc().getElementById("mainframe");
   mainFrame=new ViewPort("mainframe",0,0,w*pt_scale,h*pt_scale);
   mainFrame.element=el;
   mainFrame.container=el;
   mainFrame.map(0,0,1,1);
   mainFrame.appendChild(mainFrame.styledRectangle(-0.1,-0.1,1.2,1.2,
     svgColor(background,"fill")));
   return mainFrame;
  };
  function ViewPort(id,x,y,w,h)
  {
   var i;
   this.origin=new Array;
   this.size=new Array;
   this.offset=new Array;
   this.scale=new Array;
   this.origin[0]=x;
   this.origin[1]=y;
   this.size[0]=w;
   this.size[1]=h;
   this.offset[0]=0;
   this.scale[0]=w;
   this.offset[1]=-1;
   this.scale[1]=-h;
   this.maxXLabel=7;
   this.maxYLabel=7;
   this.gridStyle="gridline";
   this.minorpipStyle="minorpip";
   this.majorpipStyle="majorpip";
   this.borderStyle="majorpip";
   this.fnx=false;
   this.fny=false;
   if(id=="Init"){return;};
   if(ViewPortDoc())
   {
    this.element=ViewPortDoc().createElementNS("http://www.w3.org/2000/svg","g");
	this.container=this.element;
    this.element.setAttributeNS(null,"id",id);
    this.element.setAttributeNS(null,"transform",
	"translate(" + x + " " + y + ")");
   };
  };
  new ViewPort("Init",0,0,0,0);
  ViewPort.prototype.valx=function(x)
  {
   if(this.fnx)
   {
    switch(this.fnx)
    {
    case "log":
     x=Math.log(x);
     break;
    };
   };
   return x;
  };
  ViewPort.prototype.valy=function(y)
  {
   if(this.fny)
   {
    switch(this.fny)
    {
    case "log":
     y=Math.log(y);
     break;
    case "mercator":
     y=mercator(y);
     break;
    };
   };
   return y;
  };
  ViewPort.prototype.map=function(x,y,w,h,fnx,fny)
  {
   var xx,yy;
   this.fnx=fnx;this.fny=fny;
   xx=this.valx(x+w);yy=this.valy(y+h);x=this.valx(x);y=this.valy(y);
   this.offset[0]=-x;
   this.offset[1]=-yy;
   this.scale[0]=this.size[0]/(xx-x);
   this.scale[1]=-this.size[1]/(yy-y);
  };
  ViewPort.prototype.round=function(v)
  {
   return roundFactor(v,100);
  };
  ViewPort.prototype.ux=function(x)
  {
   x=this.valx(x);
   return this.round((x+this.offset[0])*this.scale[0]);
  };
  ViewPort.prototype.uy=function(y)
  {
   y=this.valy(y);
   return this.round((y+this.offset[1])*this.scale[1]);
  };
  ViewPort.prototype.uw=function(w,at)
  {
   if(typeof(at)=='undefined')
   {
    return this.round(w*this.scale[0]);
   }
   else
   {
    return this.round(this.ux(at+w)-this.ux(at));
   };
   return 0;
  };
  ViewPort.prototype.uh=function(h,at)
  {
   if(typeof(at)=='undefined')
   {
    return this.round(h*this.scale[1]);
   }
   else
   {
    return this.round(this.uy(at+h)-this.uy(at));
   };
   return 0;
  };
  ViewPort.prototype.createElement=function(name)
  {
   return ViewPortDoc().createElementNS("http://www.w3.org/2000/svg",name);
  };
  ViewPort.prototype.createTextNode=function(value){return ViewPortDoc().createTextNode(value);};
  ViewPort.prototype.appendChild=function(el){this.container.appendChild(el);};
  ViewPort.prototype.group=function(classname)
  {
   var el;
   el=ViewPort.prototype.createElement("g");
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   return el;
  };
  ViewPort.prototype.image=function(x,y,w,h,url)
  {
   var el;
   el=ViewPort.prototype.createElement("image");
   if(url)
   {
    el.setAttributeNS(xlinkNS,"href",url);
   };
   el.setAttributeNS(null,"preserveAspectRatio","none");
   el.setAttributeNS(null,"x",this.ux(x));
   el.setAttributeNS(null,"width",this.uw(w,x));
   if(this.uh(h,y)<0)
   {
    el.setAttributeNS(null,"y",this.uy(y+h));
    el.setAttributeNS(null,"height",-this.uh(h,y));
   }
   else
   {
    el.setAttributeNS(null,"y",this.uy(y));
    el.setAttributeNS(null,"height",this.uh(h,y));
   }
   return el;
  };
  ViewPort.prototype.rectangle=function(x,y,w,h,classname)
  {
   var el;
   el=ViewPort.prototype.createElement("rect");
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   el.setAttributeNS(null,"x",this.ux(x));
   el.setAttributeNS(null,"width",this.uw(w,x));
   if(this.uh(h,y)<0)
   {
    el.setAttributeNS(null,"y",this.uy(y+h));
    el.setAttributeNS(null,"height",-this.uh(h,y));
   }
   else
   {
    el.setAttributeNS(null,"y",this.uy(y));
    el.setAttributeNS(null,"height",this.uh(h,y));
   }
  return el;
  };
  ViewPort.prototype.styledRectangle=function(x,y,w,h,sty)
  {
   var el;
   el=ViewPort.prototype.createElement("rect");
   el.setAttributeNS(null,"style",sty);
   el.setAttributeNS(null,"x",this.ux(x));
   el.setAttributeNS(null,"y",this.uy(y+h));
   el.setAttributeNS(null,"width",this.uw(w,x));
   el.setAttributeNS(null,"height",-this.uh(h,y));
   el.setAttributeNS(null,"stroke","none");
   return el;
  };
  ViewPort.prototype.viewPort=function(id,x,y,w,h)
  {
   var vp;
   vp=new ViewPort(id,this.ux(x),this.uy(y+h),this.uw(w,x),-this.uh(h,y));
   return vp;
  };
  ViewPort.prototype.polygon=function(id,x,y,classname)
  {
   var el,i;
   var points="";
   var pnt=new Array;
   el=ViewPort.prototype.createElement("polygon");
   if(id!="")
   {
    el.setAttributeNS(null,"id",id);
   };
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   for(i=0;i<x.length;i++)
   {
    pnt[i]=this.ux(Number(x[i]))+","+this.uy(Number(y[i]));
   };
   points=pnt.join("\n");
   el.setAttributeNS(null,"points",points);
   return el;
  };
  ViewPort.prototype.polyline=function(id,x,y,classname)
  {
   var el,i;
   var points="";
   var pnt=new Array;
   el=ViewPort.prototype.createElement("polyline");
   if(id!="")
   {
    el.setAttributeNS(null,"id",id);
   };
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   for(i=0;i<x.length;i++)
   {
    pnt[i]=this.ux(Number(x[i]))+","+this.uy(Number(y[i]));
   };
   points=pnt.join("\n");
   el.setAttributeNS(null,"points",points);
   return el;
  };
  ViewPort.prototype.mapRectangle=function(rx,ry,rw,rh,x,y,w,h,fnx,fny)
  {
   var xx,yy;
   var urx=this.ux(rx),ury=this.uy(ry),urw=this.uw(rw,rx),urh=this.uh(rh,ry);
   this.fnx=fnx;this.fny=fny;
   xx=this.valx(x+w);yy=this.valy(y+h);x=this.valx(x);y=this.valy(y);
   this.scale[0]=(urw)/(xx-x);
   this.scale[1]=(urh)/(yy-y);
   this.offset[0]=(urx/this.scale[0])-x;
   this.offset[1]=(ury/this.scale[1])-y;
   this.w=w;
   this.h=h;
  };
  ViewPort.prototype.line=function(x1,y1,x2,y2,classname)
  {
   var el;
   el=ViewPort.prototype.createElement("line");
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   el.setAttributeNS(null,"x1",this.ux(x1));
   el.setAttributeNS(null,"y1",this.uy(y1));
   el.setAttributeNS(null,"x2",this.ux(x2));
   el.setAttributeNS(null,"y2",this.uy(y2));
   return el;
  };
  ViewPort.prototype.circle=function(x,y,r,classname)
  {
   var el;
   el=ViewPort.prototype.createElement("circle");
   if(classname)
   {
    el.setAttributeNS(null,"class",classname);
   };
   if(r)
   {
    el.setAttributeNS(null,"r",this.round(r));
   };
   el.setAttributeNS(null,"cx",this.ux(x));
   el.setAttributeNS(null,"cy",this.uy(y));
   return el;
  };
  ViewPort.prototype.symbol=function(x,y,r,symbolname)
  {
   var el,points,subel;
   if(!r){r=3.75;};x=this.ux(x);y=this.uy(y);
   r*=font_scale;
   switch(symbolname)
   {
   default:
   case "circle":
    el=ViewPort.prototype.createElement("circle");
    el.setAttributeNS(null,"r",this.round(r));
    el.setAttributeNS(null,"cx",x);
    el.setAttributeNS(null,"cy",y);
    break;
   case "square":
    r=r*0.79;
    el=ViewPort.prototype.createElement("polygon");
    points=this.round(x-r)+","+this.round(y-r)+" "+this.round(x+r)+","+this.round(y-r)+" "+this.round(x+r)+","+this.round(y+r)+" "+this.round(x-r)+","+this.round(y+r);
    el.setAttributeNS(null,"points",points);
    break;
   case "diamond":
    r=r*1.1;
    el=ViewPort.prototype.createElement("polygon");
    points=this.round(x-r)+","+this.round(y)+" "+this.round(x)+","+this.round(y-r)+" "+this.round(x+r)+","+this.round(y)+" "+this.round(x)+","+this.round(y+r);
    el.setAttributeNS(null,"points",points);
    break;
   case "triangle":
    r=r*0.91;
    el=ViewPort.prototype.createElement("polygon");
    points=this.round(x-r)+","+this.round(y+r/2)+" "+this.round(x+r)+","+this.round(y+r/2)+" "+this.round(x)+","+this.round(y-0.86*r);
    el.setAttributeNS(null,"points",points);
    break;
   case "cross":
    r=r*1.1;
    el=ViewPort.prototype.createElement("g");
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",this.round(x-r));
    subel.setAttributeNS(null,"y1",y);
    subel.setAttributeNS(null,"x2",this.round(x+r));
    subel.setAttributeNS(null,"y2",y);
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x);
    subel.setAttributeNS(null,"y1",this.round(y-r));
    subel.setAttributeNS(null,"x2",x);
    subel.setAttributeNS(null,"y2",this.round(y+r));
    el.appendChild(subel);
    break;
   case "x":
    r=r*0.79;
    el=ViewPort.prototype.createElement("g");
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",this.round(x-r));
    subel.setAttributeNS(null,"y1",this.round(y-r));
    subel.setAttributeNS(null,"x2",this.round(x+r));
    subel.setAttributeNS(null,"y2",this.round(y+r));
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",this.round(x-r));
    subel.setAttributeNS(null,"y1",this.round(y+r));
    subel.setAttributeNS(null,"x2",this.round(x+r));
    subel.setAttributeNS(null,"y2",this.round(y-r));
    el.appendChild(subel);
    break;
   };
//   el.setAttributeNS(null,"style","stroke:black;stroke-width:1px;fill:white;");
   return el;
  };
  ViewPort.prototype.errorbars=function(x,y,dx,dy,r)
  {
   var el,x1,x2,y1,y2;
   if(!r){r=2;};
   x1=this.ux(x-dx);
   x2=this.ux(x+dx);
   y1=this.uy(y-dy);
   y2=this.uy(y+dy);
   if((x1==x2)&&(y1==y2)){return this.symbol(x,y,0,"cross");};
   x=this.ux(x);
   y=this.uy(y);
   el=ViewPort.prototype.createElement("g");
   if(x1!=x2)
   {
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x1);
    subel.setAttributeNS(null,"y1",y);
    subel.setAttributeNS(null,"x2",x2);
    subel.setAttributeNS(null,"y2",y);
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x1);
    subel.setAttributeNS(null,"y1",y-r);
    subel.setAttributeNS(null,"x2",x1);
    subel.setAttributeNS(null,"y2",y+r);
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x2);
    subel.setAttributeNS(null,"y1",y-r);
    subel.setAttributeNS(null,"x2",x2);
    subel.setAttributeNS(null,"y2",y+r);
    el.appendChild(subel);
   };
   if(y1!=y2)
   {
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x);
    subel.setAttributeNS(null,"y1",y1);
    subel.setAttributeNS(null,"x2",x);
    subel.setAttributeNS(null,"y2",y2);
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x-r);
    subel.setAttributeNS(null,"y1",y1);
    subel.setAttributeNS(null,"x2",x+r);
    subel.setAttributeNS(null,"y2",y1);
    el.appendChild(subel);
    subel=ViewPort.prototype.createElement("line");
    subel.setAttributeNS(null,"x1",x-r);
    subel.setAttributeNS(null,"y1",y2);
    subel.setAttributeNS(null,"x2",x+r);
    subel.setAttributeNS(null,"y2",y2);
    el.appendChild(subel);
   };
//   el.setAttributeNS(null,"style","stroke:black;stroke-width:1px;fill:white;");
   return el;
  };
  ViewPort.prototype.text=function(x,y,str,size,anchor,shift,fill,style)
  {
   var el,sty,ar,i,j,pts,levs,lev,ctrl,ln;
   el=this.createElement("text");
   sty="";
   if(size)
   {
    sty="font-size:"+font_scale*size+"pt";
    if(!isNaN(shift) && shift)
    {  
	 shift*=0.8;
     el.setAttributeNS(null,"transform","translate(0 "
          +this.round(-size*shift*line_scale)+")");
    };
   };
   if(anchor)
   {
    if(sty){sty=sty+";";};
    sty=sty+"text-anchor:"+anchor;
   };
   if(style)
   {
    if(sty){sty=sty+";";};
    sty=sty+"font-style:"+style;
   };
   if(fill)
   {
    if(sty){sty=sty+";";};
    sty=sty+"fill:"+fill;
   };
   if(sty)
   {
    el.setAttributeNS(null,"style",sty);
   };
   el.setAttributeNS(null,"x",this.ux(x));
   el.setAttributeNS(null,"y",this.uy(y));
   ar=str.toString().split("{");
   if(ar.length==1)
   {
    el.appendChild(ViewPort.prototype.createTextNode(str));
   }
   else
   {
    ln=0;levs=[];levs[0]=el;lev=0;
    for(i=0;i<ar.length;i++)
    {
     pts=ar[i].split("}");
     for(j=0;j<pts.length-1;j++)
     {
      if(lev<0){return el;};
      levs[lev].appendChild(ViewPort.prototype.createTextNode(pts[j]));
      lev--;
     };
     cntrl='';
     if(pts[j].length!=0)
     {
      switch(pts[j].charAt(pts[j].length-1))
      {
      case '_': 
      case '^':
       cntrl=pts[j].charAt(pts[j].length-1);
       pts[j]=pts[j].substr(0,pts[j].length-1);
       break;
      default :
       if(pts[j].lastIndexOf('\\')!=-1)
       {
        cntrl=pts[j].substr(pts[j].lastIndexOf('\\'))
        pts[j]=pts[j].substr(0,pts[j].lastIndexOf('\\'));
       };
       break;
      };
     };
     levs[lev].appendChild(ViewPort.prototype.createTextNode(pts[j]));
     if(i==ar.length-1){continue;};
     lev++;
     levs[lev]=ViewPort.prototype.createElement("tspan");
     levs[lev-1].appendChild(levs[lev]);
     switch(cntrl)
     {
     case '^':
      levs[lev].setAttributeNS(null,"baseline-shift","super");
      levs[lev].setAttributeNS(null,"font-size","smaller");
      break;
     case '_':
      levs[lev].setAttributeNS(null,"baseline-shift","sub");
      levs[lev].setAttributeNS(null,"font-size","smaller");
      break;
     case '\\textit': case '\\it':
      levs[lev].setAttributeNS(null,"font-style","italic");
      break;
     case '\\textbf': case '\\bf':
      levs[lev].setAttributeNS(null,"font-weight","bold");
      break;
     default:
      break;
     };
    };
   };
   return el;
  };
  ViewPort.prototype.rotatedText=function(x,y,degrees,strn,size,anchor,shift,fill,style)
  {
   var el;
   var at;
   var rad=degrees*Math.PI/180;
   var cs=roundFactor(Math.cos(rad),1000);
   var sn=roundFactor(Math.sin(rad),1000);
   el=this.text(x,y,strn,size,anchor,shift,fill,style);
   at="matrix("+cs+","+(sn)+","+(-sn)+","+cs+","+this.ux(x)+","+this.uy(y)+")";
   el.setAttributeNS(null,"transform",at);
   el.setAttributeNS(null,"x",0);
   el.setAttributeNS(null,"y",0);
   if(size)
   {
    if(!isNaN(shift) && shift)
    {  
	 shift*=0.8;
     el.setAttributeNS(null,"y",this.round(-size*shift*line_scale));
    };
   };
   return el;
  };
  ViewPort.prototype.xlabel="";
  ViewPort.prototype.ylabel="";
  ViewPort.prototype.xlabeller=function(pos,gap)
  {
   var test,fact,i;
   fact=1;
   test=pos;
   for(i=0;gap<1;i++)
   {
    fact*=10;
    test*=10;
    gap*=10;
   };
   return roundFactor(pos,fact)
  };
  ViewPort.prototype.ylabeller=ViewPort.prototype.xlabeller;
  ViewPort.prototype.pipFinder=function(val)
  {
   var ten,pwr,res;
   if(val>0)
   {
    ten=Math.log(val)/Math.LN10;
    pwr=Math.floor(ten); // power to raise major and minor by
    res=Math.pow(10,ten-pwr); // number that should be in range 1 to 9.9999
    if(res>5){return 10*Math.pow(10,pwr);};
    if(res>2){return 5*Math.pow(10,pwr);};
    return 2*Math.pow(10,pwr);
   };
   return 1000;
  };
  ViewPort.prototype.axis=function(type,from,to,major,minor,fill,style)
  {
   var pos;
   var g;
   g=this.createElement("g");
   switch(type)
   {
   case "x":case "xt":
    if(this.xlabel=="noaxis"){return;};
    break;
   case "y":case "yr":
    if(this.ylabel=="noaxis"){return;};
    break;
   };
   switch(type)
   {
   case "x":
    g.appendChild(this.line(from,0,to,0,this.borderStyle));
    break;
   case "xt":
    g.appendChild(this.line(from,this.h,to,this.h,this.borderStyle));
    break;
   case "y":
    g.appendChild(this.line(0,from,0,to,this.borderStyle));
    break;
   case "yr":
    g.appendChild(this.line(this.w,from,this.w,to,this.borderStyle));
    break;
   };
   if((!major)||(major==0))
   {
    switch(type)
    {
    case "x":case "xt":
     major=this.pipFinder((to-from)/this.maxXLabel);
     minor=this.pipFinder(major/10.1);
     break;
    case "y":case "yr":
     major=this.pipFinder((to-from)/this.maxYLabel);
     minor=this.pipFinder(major/10.1);
     break;
    };
   };
   for(pos=Math.ceil(from/minor)*minor;pos<=to;pos+=minor)
   {
    switch(type)
    {
    case "x":
     g.appendChild(this.line(pos,0,pos,0.1,this.minorpipStyle));
     break;
    case "xt":
     g.appendChild(this.line(pos,this.h,pos,this.h-0.1,this.minorpipStyle));
     break;
    case "y":
     g.appendChild(this.line(0,pos,0.1,pos,this.minorpipStyle));
     break;
    case "yr":
     g.appendChild(this.line(this.w,pos,this.w-0.1,pos,this.minorpipStyle));
     break;
    };
   };
   for(pos=Math.ceil(from/major)*major;pos<=to;pos+=major)
   {
    switch(type)
    {
    case "x":
     g.appendChild(this.line(pos,0,pos,0.2,this.majorpipStyle));
     g.appendChild(this.text(pos,0,this.xlabeller(pos,major),10,"middle",-1.45));
     break;
    case "xt":
     g.appendChild(this.line(pos,this.h,pos,this.h-0.2,this.majorpipStyle));
     g.appendChild(this.text(pos,this.h,this.xlabeller(pos,major),10,"middle",0.45));
     break;
    case "y":
     g.appendChild(this.line(0,pos,0.2,pos,this.majorpipStyle));
     g.appendChild(this.text(-0.1,pos,this.ylabeller(pos,major),10,"end",-0.5));
     break;
    case "yr":
     g.appendChild(this.line(this.w,pos,this.w-0.2,pos,this.majorpipStyle));
     g.appendChild(this.text(this.w+0.1,pos,this.ylabeller(pos,major),10,"start",-0.5));
     break;
    };
   };
   switch(type)
   {
   case "x":
    if(this.xlabel!="nonumbers")
    {
     g.appendChild(this.text((from+to)/2,-1.0,this.xlabel,10,"middle",
     	-0.5,fill,style));
    };
    break;
   case "xt":
    if(this.xlabel!="nonumbers")
    {
     g.appendChild(this.text((from+to)/2,this.h+1.0,this.xlabel,10,"middle",
     	+0.5,fill,style));
    };
    break;
   case "y":
    if(this.ylabel!="nonumbers")
    {
     g.appendChild(this.rotatedText(-2.0,(from+to)/2,-90,this.ylabel,
     	10,"middle",-0.5,fill,style));
    };
    break;
   case "yr":
    if(this.ylabel!="nonumbers")
    {
     g.appendChild(this.rotatedText(this.w+2.0,(from+to)/2,90,this.ylabel,
     	10,"middle",-0.5,fill,style));
    };
    break;
   };
   this.appendChild(g);
  };
  ViewPort.prototype.grid=function(type,from,to,max,major)
  {
   var pos;
   var g;
   g=this.createElement("g");
   if(!major)
   {
    if(type=="x")
    {
     major=this.pipFinder((to-from)/this.maxXLabel);
    };
    if(type=="y")
    {
     major=this.pipFinder((to-from)/this.maxYLabel);
    };
   };
   for(pos=Math.ceil(from/major)*major;pos<=to;pos+=major)
   {
    if(type=="x")
    {
     g.appendChild(this.line(pos,0,pos,max,this.gridStyle));
    };
    if(type=="y")
    {
     g.appendChild(this.line(0,pos,max,pos,this.gridStyle));
    };
   };
   this.appendChild(g);
  };
  ViewPort.prototype.mapPlot=function(x,y,w,h,nx,ny,opacityFunction,style)
  {
   var el,i,j,g;
   el=ViewPort.prototype.createElement("rect");
   g=ViewPort.prototype.createElement("g");
   g.setAttributeNS(null,"style",style);
   el.setAttributeNS(null,"width",Math.ceil(this.uw(w,x)/(nx-1)));
   el.setAttributeNS(null,"height",Math.ceil(-this.uh(h,y)/(ny-1)));
   el.setAttributeNS(null,"class","fill");
   x-=0.5*w/(nx-1);
   y-=0.5*h/(ny-1);
   for(i=0;i<nx;i++)
   {
    for(j=0;j<ny;j++)
    {
     el=el.cloneNode(false);   
     el.setAttributeNS(null,"opacity",opacityFunction(i,j));
     el.setAttributeNS(null,"x",this.ux(x+i*w/(nx-1)));
     el.setAttributeNS(null,"y",this.uy(y+(j+1)*h/(ny-1)));
     g.appendChild(el);
    };
    this.appendChild(g);
   };
  };
  var clipCount=0;
  ViewPort.prototype.clipPath=function(el)
  {
   var cp,g;
   cp=this.createElement("clipPath");
   clipCount++;
   cp.setAttributeNS(null,"id","clip"+clipCount);
   cp.appendChild(el);
   ViewPortDoc().getElementById("definitions").appendChild(cp);
// new method
   g=this.createElement("g");
   g.setAttributeNS(null,"clip-path","url(#clip"+clipCount+")");
   this.container=g;
   this.element.appendChild(this.container);
//   this.element.setAttributeNS(null,"clip-path","url(#clip"+clipCount+")");
  };


