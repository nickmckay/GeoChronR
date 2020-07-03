 var ocd=parent.ocd;
 var calib=parent.calib;
 var plotOptions=parent.plotOptions;
 var oct=new Array();
 
 function clearCsv()
 {
  parent.filecontent="";
 };
 
 function addToCsv(item)
 {
  switch(typeof(item))
  {
  case "string":
   parent.filecontent+='"'+item+'"'+',';
   break;
  default:
   parent.filecontent+=item+',';
   break;
  };
 };
 
 function skipCsv(no)
 {
  var i;
  for(i=0;i<no;i++){parent.filecontent+=',';};
 };
 
 function endCsv()
 {
  parent.filecontent+='\n';
 };
 
 function makeVisible(i,vis)
 {
  var j,st;
  if(vis)
  {
   st="table-row";
   if(parent.isInternetExplorer())
   {
    st="block";
   };
   oct[i].tableRow.style.display=st;
   if(oct[i].errorRow)
   {
    oct[i].errorRow.style.display=st;
   };
  }
  else
  {
   oct[i].tableRow.style.display="none";
   if(oct[i].errorRow)
   {
    oct[i].errorRow.style.display="none";
   };
  };
  ocd[i].hidden=!vis;
  if(!ocd[i].collapsed)
  {
   for(j=0;j<ocd[i].children.length;j++)
   {
    makeVisible(ocd[i].children[j],vis);
   };
  };
 };
 
 function containsStructure(i)
 {
  var j;
  for(j=0;j<ocd[i].children.length;j++)
  {
   if(ocd[ocd[i].children[j]].children.length)
   {
    if((ocd[ocd[i].children[j]].op=="Calculate")||(ocd[ocd[i].children[j]].op=="Interval"))
    {
    }
    else
    {
     return true;
    };
   };
  };
  return false;
 };
 
 function expand(i,propagate)
 {
  var vis,j;
  ocd[i].collapsed=!(ocd[i].collapsed);
  vis=!(ocd[i].collapsed);
  if(i)
  {
   if(vis)
   {
    if(plotOptions.showReversed)
    {
     oct[i].tableExpand.src="../img/SmallUp.gif";
    }
    else
    {
     oct[i].tableExpand.src="../img/SmallDown.gif";
    };
   }
   else
   {
    oct[i].tableExpand.src="../img/SmallRight.gif";
   };
  };
  for(j=0;j<ocd[i].children.length;j++)
  {
   if(ocd[ocd[i].children[j]].level)
   {
    makeVisible(ocd[i].children[j],vis);
   };
   if(propagate && ((propagate != "structure")||(containsStructure(ocd[i].children[j]))))
   {
    if(ocd[ocd[i].children[j]].children.length)
    {
     if(ocd[i].collapsed!=ocd[ocd[i].children[j]].collapsed)
     {
      expand(ocd[i].children[j],propagate);
     };
    };
   };
  };
 };
 
 function expandAll(i)
 {
  expand(i,true);
 };
 
 function showSelectNo(i)
 {
  var s,txt="";
  if(!oct[i]){return;};
  s=oct[i].tableSelectNo;
  if(ocd[i].selectNo)
  {
   txt+=ocd[i].selectNo;
  };
  txt=document.createTextNode(txt);
  if(s.firstChild)
  {
   s.replaceChild(txt,s.firstChild);
  }
  else
  {
   s.appendChild(txt);
  };
 };
 
 function showAllSelectNo()
 {
  parent.sortSelection()
  for(i=0;i<oct.length;i++)
  {
   showSelectNo(i);
  };
 };
 
 function checkSelect(i)
 {
  if(oct[i].tableSelect.checked)
  {
   parent.selectNo++;
   ocd[i].selectNo=parent.selectNo;
  }
  else
  {
   if((ocd[i].selectNo==parent.selectNo)&&(parent.selectNo))
   {
    parent.selectNo--;
   };
   ocd[i].selectNo=0;
  };
  showSelectNo(i);
 };
  
 function doSelect(i,value)
 {
  if(!oct[i]){return;};
  oct[i].tableSelect.checked=value;
  if(value)
  {
   parent.selectNo++;
   ocd[i].selectNo=parent.selectNo;
  }
  else
  {
   if((ocd[i].selectNo==parent.selectNo)&&(parent.selectNo))
   {
    parent.selectNo--;
   };
   ocd[i].selectNo=0;
  };
  showSelectNo(i);
 };
 
 function checkGroup(i,value)
 {
  var j;
  doSelect(i,value);
  for(j=0;j<ocd[i].children.length;j++)
  {
   if(!ocd[ocd[i].children[j]].hidden)
   {
    checkGroup(ocd[i].children[j],value);
   };
  };
 };

 
 function checkNewPage(i)
 {
  ocd[i].newPage=oct[i].tableNewPage.checked;
 };
 
 function showAll()
 {
  expandAll(0);
 };
 
 function showStructure()
 {
  ocd[0].collapsed=false;
  expandAll(0);  
  expand(0,"structure");
};
 
 function selectAll()
 {
  var si,i;
  var tovalue=true;
  var this_is_first=true;
  parent.selectNo=0;
  for(i=1;i<ocd.length;i++)
  {
   if(!ocd[i]){continue;};
   if(this_is_first)
   {
    si=oct[i].tableSelect;
    if(si)
    {
     if(this_is_first && si.checked){tovalue=false;this_is_first=false;};
     doSelect(i,tovalue);
    };
   }
   else
   {
    doSelect(i,tovalue);
   };
  };
 };
 
 function selectVisible()
 {
  var si,i,j;
  var tovalue=true;
  parent.selectNo=0;
  for(i=1;i<ocd.length;i++)
  {
   if(!ocd[i]){continue;};
   doSelect(i,!ocd[i].hidden);
  };
  if(tovalue){parent.selectNo=ocd.length;}else{parent.selectNo=0;};
 };
 
 function appendText(str,el)
 {
  el.appendChild(document.createTextNode(str));
  return el;
 };
 
 function createTD(str)
 {
  return appendText(str,document.createElement("TD"));
 };

 function createTH(str)
 {
  return appendText(str,document.createElement("TH"));
 };
 
 function tableRange(d,j,i,k,el)
 {
  var dist,count,m,n,tot,r,intercept,p,td,str;
  if(j==0){dist=ocd[i].likelihood;}else{dist=ocd[i].posterior;};
  if(!plotOptions.showRange[k]){return;};
  intercept=false;p=0;
  if(dist && dist.range && dist.range[k])
  {
   r=simplifyDateRange(dist.range[k],ocd[i].type,plotOptions.showWhole);
   if(r)
   {
    count=r.length;
    for(n=0;n<count;n++)
    {
     p+=r[n][2];
    };
    if(p<2){intercept=true;};
    for(m=0;m<3;m++)
    {
     str="";
     td=document.createElement("TD");
     for(n=0;n<count;n++)
     {
      if(m<2)
      {
       if(dist.range[k][n][m]=="...")
       {
        appendText("...",td);
        str+="...";
       }
       else
       {
        appendText(r[n][m],td);
        str+=r[n][m];
        if(n<count-1)
        {
         td.appendChild(document.createElement("BR"));
         str+="\n";
        };
       };
      }
      else
      {
       if(intercept)
       {
        appendText(toFixxed((r[n][m]*100),1),td);
        str+=r[n][m]*100;
        if(n<count-1)
        {
         td.appendChild(document.createElement("BR"));
         str+="\n";
        };
       }
       else
       {
        appendText(toFixxed((r[n][m]),1),td);
        str+=r[n][m];
        if(n<count-1)
        {
         td.appendChild(document.createElement("BR"));
         str+="\n";
        };
       };
      };
     };
     addToCsv(str);
     el.appendChild(td);
    };
    return;
   };
  };
  skipCsv(3);
  el.appendChild(document.createElement("TD"));
  el.appendChild(document.createElement("TD"));
  el.appendChild(document.createElement("TD"));
 };
 
 function sumStat(d,j,i,stat,el)
 {
  var str="",td;
  if(j==0){dist=ocd[i].likelihood;}else{dist=ocd[i].posterior;};
  td=document.createElement("TD");
  if(dist && typeof(dist[stat])!='undefined')
  {
   switch(stat)
   {
   case 'sigma':
    if(ocd[i].type=='date')
    {
     str=showDateT(dist[stat],'interval',0);
     addToCsv(str);
     appendText(str,td);
     break;
    };
   default:
    str=showDateT(dist[stat],ocd[i].type,0);
    addToCsv(str);
    appendText(str,td);
    break;
   };
  }
  else
  {
   addToCsv(str); // add blank space
  };
  el.appendChild(td);
 };
 
 function goRawData()
 {
  parent.rawData(this.ind);
 };

 function goPlotData()
 {
  parent.goPlotData(this.ind);
 };
 
 function doExpand()
 {
  expand(this.ind);
 };
 
 function doExpandAll()
 {
  expandAll(this.ind);
 };
 
 function doCheckSelect()
 {
  checkSelect(this.ind);
 };
 
 function doCheckGroup()
 {
  checkGroup(this.ind,!oct[this.ind].tableSelect.checked);
  showAllSelectNo();
 };
 
 function doCheckNewPage()
 {
  checkNewPage(this.ind);
 };
 
 function for_start(){if(!plotOptions.showReversed){return 1;};return ocd.length-1;};
 function for_test(i){if(!plotOptions.showReversed){return i<ocd.length;};return i>0;};
 function for_next(i){if(!plotOptions.showReversed){return i+1;};return i-1;};
 
 function drawTable()
 {
  var rangeColumns;
  var units,str,sty,sub;
  var odd=true;
  var i,j,k,d,tab,tb,tr,td,sb,img;
  if(plotOptions.viewType!='table')
  {
   parent.updateView();
   return;
  };
  switch(plotOptions.reportingStyle)
  {
  case 0:
   units="(BP)";break;
  case 1:
   units="(BC/AD)";break;
  case 2:
   units="(BCE/CE)";break;
  case 3:
   units="(\u00B1CE)";break;
  case 4:
   units="(G)";break;
  default:
   units="";
  };
  rangeColumns=0;
  for(k=1;k<4;k++){if(plotOptions.showRange[k]){rangeColumns+=3;};};
  if(plotOptions.showMean){rangeColumns+=1;};
  if(plotOptions.showSigma){rangeColumns+=1;};
  if(plotOptions.showMedian){rangeColumns+=1;};
  if(rangeColumns==0){rangeColumns=1;};
  plotOptions.viewType="table";
  // if no data return
  if(!ocd[0]){return;};
  d=document;
  clearCsv();
  tab=document.createElement("TABLE");
  tab.className='param';
  tb=document.createElement("TBODY");
  tb.id='tb';
  tr=document.createElement("TR");
  tr.className='header';
  td=document.createElement("TH");
  td.rowSpan=2;
  appendText("Name",td);
  if(parent.showControls)
  {
   td.appendChild(document.createElement("BR"));
   td.appendChild(document.createElement("BR"));
   sub=document.createElement("SPAN");
   sub.className="linked";
   sub.onclick=showAll;
   appendText("Show all",sub);
   td.appendChild(sub);
   td.appendChild(document.createElement("BR"));
   sub=document.createElement("SPAN");
   sub.className="linked";
   sub.onclick=showStructure;
   appendText("Show structure",sub);
   td.appendChild(sub);
  };
  tr.appendChild(td);
  addToCsv("Name");
  if(parent.showControls)
  {
   td=document.createElement("TH");
   td.rowSpan=2;
   td.ind=0;
   td.onclick=goRawData;
   img=document.createElement("IMG");
   td.appendChild(img);
   if((ocd[0].likelihood && ocd[0].likelihood.warning)||(ocd[0].posterior && ocd[0].posterior.warning))
   {
    if(parent.showControls)
    {
     img.src="../img/RawError.gif";
    };
   }
   else
   {
    if(parent.showControls)
    {
     img.src="../img/Raw.gif";
    };
   };
   tr.appendChild(td);
  };
  if(!parent.tableDrawn)
  {
   ocd[0].children=new Array();
  };
  if(plotOptions.showLikelihood)
  {
   td=document.createElement("TH");
   td.colSpan=rangeColumns;
   appendText("Unmodelled "+units,td);
   addToCsv("Unmodelled "+units);skipCsv(rangeColumns-1);
   tr.appendChild(td);
  };
  if(plotOptions.showPosterior)
  {
   td=document.createElement("TH");
   td.colSpan=rangeColumns;
   appendText("Modelled "+units,td);
   addToCsv("Modelled "+units);skipCsv(rangeColumns-1);
   tr.appendChild(td);
  };
  if(plotOptions.showIndices)
  {
   td=document.createElement("TH");
   td.colSpan=5;
   appendText("Indices",td);
   str="Indices"
   if(ocd[0].posterior.modelAgreement)
   {
    td.appendChild(document.createElement("BR"));
    appendText("A",td);
    sb=document.createElement("SUB");
    appendText("model",sb);
    td.appendChild(sb);
    appendText("="+ocd[0].posterior.modelAgreement,td);
    str+="\nAmodel "+ocd[0].posterior.modelAgreement;
   };
   if(ocd[0].posterior.overallAgreement)
   {
    td.appendChild(document.createElement("BR"));
    appendText("A",td);
    sb=document.createElement("SUB");
    appendText("overall",sb);
    td.appendChild(sb);
    appendText("="+ocd[0].posterior.overallAgreement,td);
    str+="\nAoverall "+ocd[0].posterior.overallAgreement;
   };
   tr.appendChild(td);
   addToCsv(str);
  };
  if(parent.showControls)
  {
   td=createTH("Select");
   td.rowSpan=2;
   td.appendChild(document.createElement("BR"));
   td.appendChild(document.createElement("BR"));
   sub=document.createElement("SPAN");
   sub.className="linked";
   sub.onclick=selectAll;
   appendText("All",sub);
   td.appendChild(sub);
   td.appendChild(document.createElement("BR"));
   sub=document.createElement("SPAN");
   sub.className="linked";
   sub.onclick=selectVisible;
   appendText("Visible",sub);
   td.appendChild(sub);
   tr.appendChild(td);
   td=createTH("Page");
   td.appendChild(document.createElement("BR"));
   appendText("break",td);
   td.rowSpan=2;
   tr.appendChild(td);
  }
  tb.appendChild(tr);
  endCsv();
  tr=document.createElement("TR");
  tr.className="header";
  skipCsv(1);
  for(j=0;j<2;j++)
  {
   if((j==0)&&(!plotOptions.showLikelihood)){continue;};
   if((j==1)&&(!plotOptions.showPosterior)){continue;};
   if(rangeColumns==1)
   {
    tr.appendChild(createTH(""));
    skipCsv(1);
   };
   for(k=1;k<4;k++)
   {
    if(plotOptions.showRange[k])
    {
     tr.appendChild(createTH("from"));
     tr.appendChild(createTH("to"));
     tr.appendChild(createTH("%"));
     addToCsv("from");addToCsv("to");addToCsv("%");
    };
   };
   if(plotOptions.showMean){tr.appendChild(createTH("\u03BC"));addToCsv("mu");};
   if(plotOptions.showSigma){tr.appendChild(createTH("\u03C3"));addToCsv("sigma");};
   if(plotOptions.showMedian){tr.appendChild(createTH("m"));addToCsv("median");};
  };
  if(plotOptions.showIndices)
  {
   td=createTH("A");
   sb=document.createElement("SUB");
   appendText("comb",sb);
   td.appendChild(sb);
   tr.appendChild(td);
   addToCsv("Acomb");
   tr.appendChild(createTH("A"));addToCsv("A");
   tr.appendChild(createTH("L"));addToCsv("L");
   tr.appendChild(createTH("P"));addToCsv("P");
   tr.appendChild(createTH("C"));addToCsv("C");
  };
  tb.appendChild(tr);
  endCsv();
  i=0;
  if(ocd[i])
  {
   if((ocd[i].likelihood && ocd[i].likelihood.warning)||(ocd[i].posterior && ocd[i].posterior.warning))
   {
    tr=document.createElement("TR");
    tr.className='model';
    tr.appendChild(createTD(""));
    if(parent.showControls)
    {
     tr.appendChild(createTD(""));
    };
    if(plotOptions.showLikelihood)
    {
     td=document.createElement("TD");
     td.colSpan=rangeColumns;
     td.className='warning';
     if(ocd[i].likelihood&&ocd[i].likelihood.warning)
     {
      for(j=0;j<ocd[i].likelihood.warning.length;j++)
      {
       if(j>0){td.appendChild(document.createElement("BR"));};
       appendText(ocd[i].likelihood.warning[j],td);
      };
     };
     tr.appendChild(td);
    };
    if(plotOptions.showPosterior)
    {
     td=document.createElement("TD");
     td.colSpan=rangeColumns;
     td.className='warning';
     if(ocd[i].posterior&&ocd[i].posterior.warning)
     {
      for(j=0;j < ocd[i].posterior.warning.length;j++)
      {
       if(j>0){td.appendChild(document.createElement("BR"));};
       appendText(ocd[i].posterior.warning[j],td);
      };
     };
     tr.appendChild(td);
    };
    if(plotOptions.showIndices)
    {
     td=document.createElement("TD");
     td.colSpan=5;
     tr.appendChild(td);
    };
    if(parent.showControls)
    {
     td=document.createElement("TD");
     td.colSpan=2;
     tr.appendChild(td);
    };
    tb.appendChild(tr);
   };
  };
  for(i=for_start();for_test(i);i=for_next(i))
  {
   if(!ocd[i]){continue;};
   oct[i]=new Object();
   if((ocd[i].op=="Calculate")||(ocd[i].op=="Depth_Model")||(ocd[i].hidden))
   {
    ocd[i].collapsed=true;
   };
   if(!parent.tableDrawn)
   {
    if(ocd[i].level==0){ocd[0].children.push(i);};
    ocd[i].children=new Array();
    for(j=i+1;j < ocd.length;j++)
    {
     if(!ocd[j]){continue;};
     if(ocd[j].level==ocd[i].level+1)
     {
      ocd[i].children.push(j);
      if(ocd[i].collapsed)
      {
       ocd[j].hidden=true;
      };
     }
     else
     {
      if(ocd[j].level<=ocd[i].level)
      {
       break;
      };
     };
    };
   };
   sty="";
   tr=document.createElement("TR");
   oct[i].tableRow=tr;
   td=document.createElement("TD");
   img=document.createElement("IMG");
   img.src='../img/SmallSpace.gif';
   td.appendChild(img);
   if(ocd[i].hidden){ tr.style.display='none'; };
   if(ocd[i].children && ocd[i].children.length)
   {
    img.width=12*ocd[i].level;
    img.height=12;
    tr.className='model';
    img=document.createElement("IMG");
    img.src='../img/SmallSpace.gif';
    img.ind=i;
    img.onclick=doExpand;
    img.ondblclick=doExpandAll;
    oct[i].tableExpand=img;
    if(ocd[i].collapsed)
    {
     img.src='../img/SmallRight.gif';
    }
    else
    {
     if(plotOptions.showReversed)
     {
      img.src='../img/SmallUp.gif';
     }
     else
     {
      img.src='../img/SmallDown.gif';
     };
    };
    td.appendChild(img);
    odd=true;
   }
   else
   {
    img.width=12*(ocd[i].level+1);
    img.height=12;
    if(odd)
    {
     tr.className='odd';
    }
    else
    {
     tr.className='even';
    };
   };
   appendText("\xA0",td);
   sub=document.createElement("SPAN");
   sub.className='linked';
   sub.ind=i;
   if(ocd[i].order)
   {
    sub.onclick=goRawData;
   }
   else
   {
    sub.onclick=goPlotData;
   };
   str=parent.tidyText(makeTitle(ocd[i]));
   addToCsv(str);
   appendText(str,sub);
   td.appendChild(sub);
   tr.appendChild(td);
   if(parent.showControls)
   {
    td=document.createElement("TD");
    td.ind=i;
    td.onclick=goRawData
    img=document.createElement("IMG");
    if((ocd[i].likelihood && ocd[i].likelihood.warning)||(ocd[i].posterior && ocd[i].posterior.warning))
    {
     img.src="../img/RawError.gif";
    }
    else
    {
     img.src="../img/Raw.gif";
    };
    td.appendChild(img);
    tr.appendChild(td);
   };
   for(j=0;j<2;j++)
   {
    if((j==0)&&(!plotOptions.showLikelihood)){continue;};
    if((j==1)&&(!plotOptions.showPosterior)){continue;};
    if(rangeColumns==1)
    {
     skipCsv(1);
     tr.appendChild(document.createElement("TD"));
    };
    for(k=1;k<4;k++)
    {
     tableRange(d,j,i,k,tr);
    };
    if(plotOptions.showMean){sumStat(d,j,i,'mean',tr);};
    if(plotOptions.showSigma){sumStat(d,j,i,'sigma',tr);};
    if(plotOptions.showMedian){sumStat(d,j,i,'median',tr);};
   };
   if(plotOptions.showIndices)
   {
     if(ocd[i].likelihood && ocd[i].likelihood.overallAgreement)
     {
      tr.appendChild(createTD(ocd[i].likelihood.overallAgreement));
      addToCsv(ocd[i].likelihood.overallAgreement);
     }else{tr.appendChild(document.createElement("TD"));skipCsv(1);};
     if(ocd[i].posterior && ocd[i].posterior.agreement)
     {
      tr.appendChild(createTD(ocd[i].posterior.agreement));
      addToCsv(ocd[i].posterior.agreement);
     }else{tr.appendChild(document.createElement("TD"));skipCsv(1);};
     if(ocd[i].posterior && ocd[i].posterior.likelihood)
     {
      tr.appendChild(createTD(ocd[i].posterior.likelihood));
      addToCsv(ocd[i].posterior.likelihood);
     }else{tr.appendChild(document.createElement("TD"));skipCsv(1);};
     if(ocd[i].posterior && ocd[i].posterior.probability)
     {
      tr.appendChild(createTD(ocd[i].posterior.probability));
      addToCsv(ocd[i].posterior.probability);
     }else{tr.appendChild(document.createElement("TD"));skipCsv(1);};
     if(ocd[i].posterior && ocd[i].posterior.convergence)
     {
      tr.appendChild(createTD(ocd[i].posterior.convergence));
      addToCsv(ocd[i].posterior.convergence);
     }else{tr.appendChild(document.createElement("TD"));skipCsv(1);};
   };
   if(parent.showControls)
   {
    td=document.createElement("TD");
    img=document.createElement("INPUT");
    img.type='checkbox';
    img.ind=i;
    oct[i].tableSelect=img;
    img.onchange=doCheckSelect;
    img.ondblclick=doCheckGroup;
    sub=document.createElement("SUP");
    oct[i].tableSelectNo=sub;
    if(ocd[i].selectNo>0)
    {
     appendText(ocd[i].selectNo,sub);
    };
    td.appendChild(img);
    td.appendChild(sub);
    tr.appendChild(td);
    if(ocd[i].selectNo)
    {
     img.setAttribute('checked','checked');
    };
    td=document.createElement("TD");
    img=document.createElement("INPUT");
    img.type='checkbox';
    img.ind=i;
    oct[i].tableNewPage=img;
    img.onchange=doCheckNewPage;
    td.appendChild(img);
    tr.appendChild(td);
    if(ocd[i].newPage)
    {
     img.setAttribute('checked','checked');
    };
   };
   tb.appendChild(tr);
   endCsv();
   if((ocd[i].likelihood && ocd[i].likelihood.warning)||(ocd[i].posterior && ocd[i].posterior.warning))
   {
    tr=document.createElement("TR");
    if(ocd[i].hidden){ tr.style.display='none'; };
    oct[i].errorRow=tr;
    if(ocd[i].children && ocd[i].children.length)
    {
     tr.className='model';
    }
    else
    {
     if(odd)
     {
      tr.className='odd';
     }
     else
     {
      tr.className='even';
     };
    };
    tr.appendChild(document.createElement("TD"));
    if(parent.showControls)
    {
     tr.appendChild(document.createElement("TD"));
    };
    if(plotOptions.showLikelihood)
    {
     td=document.createElement("TD");
     td.colSpan=rangeColumns;
     td.className='warning';
     if(ocd[i].likelihood&&ocd[i].likelihood.warning)
     {
      for(j=0;j<ocd[i].likelihood.warning.length;j++)
      {
       if(j>0){td.appendChild(document.createElement("BR"));};
       appendText(ocd[i].likelihood.warning[j],td);
      };
     };
     tr.appendChild(td);
    };
    if(plotOptions.showPosterior)
    {
     td=document.createElement("TD");
     td.colSpan=rangeColumns;
     td.className='warning';
     if(ocd[i].posterior&&ocd[i].posterior.warning)
     {
      for(j=0;j<ocd[i].posterior.warning.length;j++)
      {
       if(j>0){td.appendChild(document.createElement("BR"));};
       appendText(ocd[i].posterior.warning[j],td);
      };
     };
     tr.appendChild(td);
    };
    if(plotOptions.showIndices)
    {
     td=document.createElement("TD");
     td.colSpan=5;
     tr.appendChild(td);
    };
    if(parent.showControls)
    {
     td=document.createElement("TD");
     td.colSpan=2;
     tr.appendChild(td);
    };
    tb.appendChild(tr);
   };
   odd=!odd;
  };
  tab.appendChild(tb);
  parent.tableDrawn=true;
  document.getElementById("tableArea").appendChild(tab);
 };
 
