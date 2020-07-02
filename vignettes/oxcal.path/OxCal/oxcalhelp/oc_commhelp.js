if(typeof(parseCommands)!='undefined')
{

parseCommands();

commandsRef["After"].help	="group function that returns a PDF for being after all of the grouped elements; in MCMC analysis defines terminus post quem (TPQ) events";
commandsRef["Age"].help		="type conversion function that returns an age or PDF for an age from an expression; uses the Year parameter";
commandsRef["Axis"].help		="plotting command that specifies the default range of the x-axis for dates";
commandsRef["Before"].help	="group function that returns a PDF for being before all of the grouped elements; in MCMC analysis defines terminus ante quem (TAQ) events";
commandsRef["Boundary"].help	="used with Sequence, P_Sequence, U_Sequence and V_Sequence; when paired with another Boundary defines a uniformly distributed group";
commandsRef["C_Date"].help	="simple function that returns a PDF for calendar date with Normally distributed errors; definition of date depends on options (BP or BC/AD): use -ve numbers for BC";
commandsRef["C_Combine"].help	="used to combine normally distributed errors defined by C_Date; the optional systematic uncertainty parameter can be used to define irreducible uncertainties";
commandsRef["C_Simulate"].help	="used for simulating Normally distributed dating methods";
commandsRef["Combine"].help	="group function that combines any number of PDFs which all give independent information on a parameter";
commandsRef["Color"].help="allows the user to define the color used for a distribution or a curve in plots; any of the css style color definitions can be used such as Red or #ff0000";
commandsRef["Comment"].help	="comment (ignored by the program)";
commandsRef["Correl_Matrix"].help ="generates a matrix of Pearson product-moment correlation coefficients; if no parameters are given the query is applied to the parent group";
commandsRef["Correlation"].help	="generates a correlation plot showing the relationship bewtween two parameters";
commandsRef["Covar_Matrix"].help ="generates a covariance matrix for the parameters referred to; if no parameters are given the query is applied to the parent group";
commandsRef["Curve"].help	="define the radiocarbon calibration curve to be used for R_Date and R_Combine";
commandsRef["Date"].help		="type conversion function that returns a date or PDF for a date from an expression";
commandsRef["Delta_R"].help	="defines the shift that is to be applied to dates before calibration; applies to the current Curve; if required a non-Normal prior can be applied to this parameter using the expression but this will only have an effect on MCMC models, for straight calibrations the mean and standard deviation of the distribution will be used";
commandsRef["D_Sequence"].help	="a sequence where the exact gaps between elements is known: for example a tree ring sequence";
commandsRef["Difference"].help	="calculates the difference between two parameters; if required a prior can be applied to this parameter using the expression";
commandsRef["End"].help	="this query can be used to find the end of a Boundary with a non-zero transition period (ie in a trapezium model); it must be used with the Transition command or it will just return the Boundary itself";
commandsRef["Exp"].help	="an exponential distribution with a defined time constant; the from and to parameters determine the slice over which the distribution is realised";
commandsRef["First"].help	="group function or query which returns the first event; if no parameters are given the query is applied to the parent group";
commandsRef["Gap"].help		="used primarily with D_Sequence (without an uncertianty) of V_Sequence (with an uncertainty) to define the gap between events; can also be used with Sequence to define a minimum gap";
commandsRef["Interval"].help	="query that finds the interval between events or groups of events in a Sequence";
commandsRef["KDE_Model"].help	="a hybrid Bayesian/frequentist model which assumes the underlying distribution is well described by a kernel density estimate";
commandsRef["KDE_Plot"].help	="query that generates a kernel density estimate during the MCMC sampling of any model; this can be used in place of Sum in cases where it might be more appropriate";
commandsRef["Label"].help	="plotting command that adds a label to a plot";
commandsRef["Last"].help		="group function or query which returns the last event; if no parameters are given the query is applied to the parent group";
commandsRef["Latitude"].help	="latitude flag for an item - used for plotting on a map using [View > Plot on Map]";
commandsRef["Longitude"].help	="longitude flag for an item - used for plotting on a map using [View > Plot on Map]";
commandsRef["Line"].help		="plotting command that adds a horizontal dividing line to a plot";
commandsRef["LnN"].help		="generates a log-normal distribution PDF; the mean and standard deviation should be given as natural logorithms as in LnN(ln(1000),ln(1.1)) for a log-normal distribution with a mean around 1000 and with a 1 sd error of 10% of this (ie approximately 100)";
commandsRef["MCMC_Sample"].help	="allows all of the MCMC samples to be saved to a file (default is MCMC_Sample.csv) at defined intervals and with a maximum number of samples taken (default 1000); the file can be found as a Document";
commandsRef["Mix_Curves"].help	="defines the mixture between two radiocarbon reservoirs; in the MCMC analysis this is constrained to be in the range 0-100% of the second reservoir; if required a non-Normal prior can be applied to this parameter using the expression but this will only have an effect on MCMC models, for straight calibrations the mean and standard deviation of the distribution will be used";
commandsRef["N"].help		="generates a Normal distribution PDF";
commandsRef["Number"].help	="type conversion function that returns a number or a PDF for a number from an expression";
commandsRef["Order"].help	="group function or query which gives the relative order of events; if no parameters are given the query is applied to the parent group";
commandsRef["Offset"].help	="offsets a distribution by a Normal distribution; superceded by expression like R_Date(2000,20)+N(10,5)";
commandsRef["Outlier"].help	="with no parameter removes an item from consideration in a model and in some cases a test is applied to see how likely the item is to be at this place in the model; using the outlier probability sets up outlier analysis based on an Outlier_Model - which can be specified using the name";
commandsRef["Outlier_Model"].help="defines the model for outliers with a probability assigned to them; the distrubution must be specified (typically T(5)) and a magnitude distribution (typically U(0,4) giving a scale of between 1 and 10000 years)";
commandsRef["P"].help		="allows the specification of complex distribution types like P(t,0,200,exp(-t/20),1); normalisation is handled automatically";
commandsRef["P_Sequence"].help	="defines depth models where deposition is assumed to be poisson distributed; the k parameter defines the number of events per unit length; the interpolation parameter is the number of interpolations per unit length - the number of iterpolated points can be increased to give a better plot but at the expense of convergence time";
commandsRef["Page"].help		="plotting command that specifies a page break";
commandsRef["Phase"].help	="unordered group of events/parameters";
commandsRef["Plot"].help		="plot grouping";
commandsRef["Pois"].help	="Poisson distribution with a defined mean; the distribution can be given an optional scale factor; if the mean is high this is close to a Normal distribution and is calculated as such over 200"
commandsRef["Position"].help	="defines the z position for elements in a depth model";
commandsRef["Prior"].help	="defines a PDF either from a file with specific data, or copied from earlier in the calculation";
commandsRef["Probability"].help	="extracts the probability at a particular value from a PDF";
commandsRef["R_Date"].help	="calculates the likelihood distribution for the calibrated date as function of radiocarbon concentration";
commandsRef["R_Combine"].help	="combines radiocarbon dates prior to calibration; the optional systematic uncertainty parameter can be used to define irreducible uncertianties";
commandsRef["R_F14C"].help	="calulates the likelihood distribution for the calibrated date as a function of radiocarbon concentration expressed as F14C";
commandsRef["R_Simulate"].help	="simulates a radiocarbon date for a particular calendar date";
commandsRef["Reservoir"].help	="applied to a Curve, defines a reservoir timeconstant for a curve";
commandsRef["Sample"].help	="generates a single sample from a PDF";
commandsRef["Sapwood"].help	="generates a PDF for the date of the last ring of a tree given only the heartwood/sapwood boundary and (optionally) any sapwood rings present";
commandsRef["Sapwood_Model"].help="defines the parameters to be used by the Sapwood command";
commandsRef["Sequence"].help	="defines an order for events and groups of events";
commandsRef["Sigma_Boundary"].help="used with Sequence to define normally distributed events when paired with another Sigma_Boundary; if paired with a Boundary the distribution is one sided";
commandsRef["Shift"].help	="calculates one distribution shifted by another; if required a prior can be applied to this parameter using the expression";
commandsRef["Span"].help		="group function or query which gives the span of events; if no parameters are given the query is applied to the parent group";
commandsRef["Start"].help	="this query can be used to find the start of a Boundary with a non-zero transition period (ie in a trapezium model); it must be used with the Transition command or it will just return the Boundary itself";
commandsRef["Sum"].help		="group function or query which gives the chance of any one of many events; if no parameters are given the query is applied to the parent group; if parameters are given acts like a Phase";
commandsRef["T"].help = "student's t-distribution with a mean of zero; the degrees of freedom (from 1 upwards) must be given and the distribution can be scaled with an optional scale parameter; tends to a Normal distribution and the number of degrees of freedom increases";
commandsRef["Tau_Boundary"].help	="used with Sequence, P_Sequence and U_Sequence to define exponentially distributed events when paired with a Boundary";
commandsRef["Top_Hat"].help	="defines a uniform PDF specified by the central point and half width";
commandsRef["Transition"].help="allows a boundary to have a none-instantaneous transition period; if required be a prior can be applied to the transition period; this function creates trapezium shaped distributions";
commandsRef["U"].help		="defines a uniform PDF specified by the start and end points";
commandsRef["U_Sequence"].help	="defines a sequence with uniform deposition rate between two Boundaries;  if Tau_Boundary or Zero_Boundary are used the deposition rate is assumed to be exactly exponential or linearly rising/falling; the interpolation parameter is the number of interpolations per unit length - the number of iterpolated points can be increased to give a better plot but at the expense of convergence time";
commandsRef["V_Sequence"].help	="defines a sequence where all of the gaps are known approximately with Normally distributed errors";
commandsRef["Year"].help		="defines the Year to be used with Ages";
commandsRef["Zero_Boundary"].help="used with Sequence, P_Sequence and U_Sequence to define events whose rate is linearly rising/falling from/to zero when paired with a Boundary";

};

function commandHelp(d,name)
{
 d.writeln(commandsRef[name].help);
};

function helpOver(id,name)
{
 var img;
 img=document.getElementById("Img"+id+name);
 hlp=document.getElementById("Exp"+id+name);
 if(hlp.style.display=="block")
 {
  img.src="../img/OrangeMinus.gif";
 }
 else
 {
  img.src="../img/GreenPlus.gif";
 };
 hlp=document.getElementById("Exp"+name);
};

function helpOut(id,name)
{
 var img;
 img=document.getElementById("Img"+id+name);
 hlp=document.getElementById("Exp"+id+name);
 if(hlp.style.display=="block")
 {
  img.src="../img/Orange.gif";
 }
 else
 {
  img.src="../img/Green.gif";
 };
};

function helpClick(id,name)
{
 var hlp;
 hlp=document.getElementById("Exp"+id+name);
 if(hlp.style.display=="block")
 {
  document.getElementById("Img"+id+name).src="../img/GreenPlus.gif";
  commandsRef[name].expanded=false;
  hlp.style.display="none";
 }
 else
 {
  document.getElementById("Img"+id+name).src="../img/OrangeMinus.gif";
  commandsRef[name].expanded=true;
  hlp.style.display="block";
 };
};

function commandSpecification(d,id,name,old,expand)
{
 d.write("<li><img id='Img"+id+name+"'src='");
 if(expand)
 {
  d.write("../img/Orange.gif");
 }
 else
 {
  d.write("../img/Green.gif");
 };
 d.write("' onmouseover='helpOver(\""+id+"\",\""+name+"\")' onmouseout='helpOut(\""+id+"\",\""+name+"\")' onclick='helpClick(\""+id+"\",\""+name+"\")'></img> ");
 commandSyntax(d,name,";",old);
 d.write("<span id='Exp"+id+name+"' style='display:");
 if(expand)
 {
  d.write("block");
 }
 else
 {
  d.write("none");
 };
 d.write("'><ul class='blank'><li>");
 commandHelp(d,name);
 d.writeln("<\/li><\/ul><\/span><\/li>");
};

function specList(d,id,names,old,expand)
{
 var i,n;
 if(names)
 {
  n=names.split(",");
 }
 else
 {
  n=commandsIndex;
 };
 d.writeln("<ul class='blank'>");
 for(i=0;i<n.length;i++)
 {
  commandSpecification(d,id,n[i],old,expand);
 };
 d.writeln("<\/ul>");
};
var helpSource=window.opener;
function helpInsert(id)
{
 var i,nd,txt,wnd;
 nd=document.getElementById(id).childNodes;
 for(i=0;(i<nd.length) && (nd[i].nodeName.toLowerCase()!='pre');i++){};
 if(i<nd.length)
 {
  txt=nd[i].firstChild.nodeValue;
 };
 if(helpSource)
 {
  if(helpSource.checkRight)
  {
   if(txt.indexOf("Plot(")!=-1)
   {
    helpSource.setCommand(txt);
   }
   else
   {
    if(helpSource.checkRight())
    {
     helpSource.right.insert(txt);
 	 helpSource.focus();
    }
    else
    {
     helpSource.setView(0);
     alert("Project manager now in right view mode - try again");
    };
   };
  }
  else
  {
   wnd=helpSource.open("../oxcal/OxCal.html?Mode=Input&","OxCal");
   helpSource=wnd;
   alert("Project manager now open - try again");
  };
 }
 else
 {
  alert("Help not opened from OxCal");
 };
};
function startCode(name)
{
  document.write("<table class='normal'><tbody><tr><th class='normal'>");
  document.write("<img src='../img/Green.gif' onmouseover='this.src=\"../img/GreenPlus.gif\"' "
    + "onmouseout='this.src=\"../img/Green.gif\"' onclick='helpInsert(\""+encodeURIComponent(name)+"\")'\/> ");
  document.write(name);
  document.write("<\/td><\/tr><tr><td><table class='clearHelp'><tbody><tr><td id='"+encodeURIComponent(name)+"'>");
};
function endCode()
{
 document.writeln("<\/td><\/tr><\/tbody><\/table><\/td><\/tr><\/tbody><\/table>");
};
function reveal(cls,nd)
{
 var i,divs;
 if(nd)
 {
  do
  {
   nd=nd.nextSibling;
  }
  while(nd.className!=cls);
  if(nd.style.display=="block")
  {
   nd.style.display="none";
  }
  else
  {
   nd.style.display="block";
  };
 }
 else
 {
  divs=document.getElementsByTagName("div");
  for(i=0;i<divs.length;i++)
  {
   if(divs[i].className==cls)
   {
    if(divs[i].style.display=="block")
    {
     divs[i].style.display="none";
    }
    else
    {
     divs[i].style.display="block";
    };
    divs[i].scrollIntoView();
   };
  };
 };
};
 function go_bib(ref)
 {
   window.location="ref.html#"+ref;
 };
 function go_ref(ref,author,year,words,reftype)
 {
  switch(reftype)
  {
  case 'article':
   window.open("http://scholar.google.com/scholar?as_q="+words+"&as_sauthors="+author+
   "&as_ylo="+year+"&as_yhi="+year+"&as_occt=title&safe=active","ref");
   break;
  case 'book':
   window.open("http://books.google.com/books?&q=intitle%3A"+
    words.split("+").join("+intitle%3A")+"+inauthor%3A"+ 
    author+"&btnG=Search+Books&as_drrb_is=b&as_miny_is="+ 
    year+"&as_maxy_is="+year,"ref");
   break;
  default:
   window.open("../db/db_ref.html?ref="+ref,"ref");
   break;
  };
 };
