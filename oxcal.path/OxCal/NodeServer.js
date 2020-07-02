var http = require('http');
var url = require('url');
var fs = require('fs');
var path = require('path');
var qs = require('querystring');
var os = require('os');
const { exec } = require('child_process');
var settings;
var server;
var setupfile=path.resolve(__dirname,"setup.json");
const PRIVATE = 'private, no-cache, no-store, must-revalidate, max-age=-1';
const PUBLIC = 'public, max-age=31536000';

// initialisation

fs.readFile(setupfile, function(err, data) {
	var set;
	if (err) {
	    settings = {"port":8080,"web":__dirname,"texdir":"","rasterizer":""}
	    settings.home=path.resolve(os.homedir(),'OxCal');
	    settings.web=__dirname;
	    switch(os.platform())
	    {
	    case "win32":
	     settings.oxcal=path.resolve(__dirname,"bin","OxCalWin.exe");
	     break;
	    case "darwin":
	     settings.oxcal=path.resolve(__dirname,"bin","OxCalMac");
	     break;
	    default:
	     settings.oxcal=path.resolve(__dirname,"bin","OxCalLinux");
	     break;
	    };
		settings.ok=false;
	}
	else
	{
		try
		{
		 set=JSON.parse(data);
		 settings=set;
		 settings.ok=true;
		}
		catch(e)
		{
         console.log(e);
 		 settings.ok=false;
		};
	};
    process.chdir(settings.web);
	server.listen(settings.port);
});

function parseCookie(txt)
{
 var i,aa,obj={};
 var a=txt.split("; ");
 for(i in a)
 {
  try
  {
   aa=a[i].split("=");
   obj[aa[0]]=decodeURIComponent(aa[1]);
  }
  catch(e){};
 };
 return obj;
};

function mimeType(ext)
{
 ext=ext.replace(".","");
 switch(ext)
 {
 case "html": case "css":
 case "log": case "txt":
 case "csv": 
 case "14i":
 case "prior": case "oxcal": 
  return "text/"+ext;
  return "text/plain";
 case "js": case "plot": 
  return "text/javascript";
 case "svg":
  return "image/svg+xml";
 case "pdf":
  return "application/pdf";
 case "png":
  return "image/png";
 case "tex":
  return "application/x-tex";
 case "json":
  return "application/json";
 };
 return "application/force-download";
};

async function readDir(dir,ext,contains,within,detail,callback)
{
  var rtn={"path":dir.replace(settings.home,"")+"/","files":[],"dirs":[]};
  var n,names,stat;
  try {
   names = await new Promise((res,err) => {
      try{fs.readdir(dir,(e,f) => {if(e){err(e)}else{res(f);}} );}
      catch (e) {err(e);} });
   if (names === undefined) {
     console.log('undefined');
   } else {
    for(n in names) {
     if(names[n].indexOf(".")==0){continue;};
     stat = await new Promise((res,err) => {
       try{fs.stat(path.join(dir,names[n]),(e,s) => {if(e){res(false)}else{res(s);}} );}
       catch (e) {res(false);} });
     if(stat==false){continue;};
     if(stat.isDirectory())
     {
      rtn.dirs.push(names[n]);
      continue;
     };
     if(stat.isFile())
     {
      if(ext && (path.extname(names[n])!="."+ext)){continue;};
      if(contains)
      {
       try
       {
/*        let fd = await new Promise(function (res,err) {
          try{fs.open(path.join(dir,names[n],"r",function (e,fd) {if(e){err(e)}else{res(fd);}} );}
          catch (e) {err(e);} });*/
       }
       catch(e){continue;};
      };
      if(detail)
      {
       rtn.files.push({"name":names[n],"size":stat.size,"updated":stat.mtime});
      }
      else
      {
       rtn.files.push(names[n]);
      };
     };
    }
   }
   callback(rtn);
  } catch (e) {
    console.log('Error', e);
  }
};

server=http.createServer(function (req, res) {
  var body="";
  var q = url.parse(req.url, true);
  var filename = path.join(".",q.pathname); 
  var query = qs.parse(q.query);
  function sendBack(err,txt)
  {
   if(err)
   {
    res.writeHead(404, {'Content-Type': mimeType("txt")});
    return res.end(err+":"+txt);
   }
   res.writeHead(200, {'Content-Type': mimeType("txt"),
		'Cache-Control':PRIVATE});
   return res.end(txt);
  };
  // deal with specials 
  switch(q.pathname)
  {
  case "/testNodeJs":
   res.writeHead(200, {'Content-Type': mimeType("txt"),
		'Cache-Control':PRIVATE});
   res.write(settings.ok.toString());
   return res.end();
  case "/setup.json":
   if(req.method=="POST")
   {
     req.on('data', function (data) {
       body += data;
       if (body.length > 1e6) { 
                // FLOOD ATTACK OR FAULTY CLIENT, NUKE REQUEST
         req.connection.destroy();
         console.log("Possible flood attack");
       }
     });
     req.on('end', function () {
       try
       {
        set=qs.parse(body);
        settings=set;
        settings.ok=true;
        console.log("Settings changed!");
        fs.writeFile(setupfile,JSON.stringify(set), function(err) {
         if(err) { return console.log(err);};
        });
 	    res.writeHead(301,{Location: './'});
        res.end();
       }
       catch(e)
       {
	    res.writeHead(301,{Location: './setup.html'});
        res.end();
        return;
       };
     });
     return;
   }
   else
   {
    fs.readFile(setupfile, function(err, data) {
     if (err) {
	  data=JSON.stringify(settings);
     };
     res.writeHead(200, {'Content-Type': mimeType("json"),'Cache-Control':PRIVATE});
     res.write(data);
     return res.end();
    });
    return;
   };
   break;
  case "/":
   if(settings.ok) {
     filename = "./index.html";
   }
   else {
     filename = "./setup.html";
   };
   break;
  };
  // treat post of a file as a content save
  if(req.method=="POST")
  {
   if(!q.query.action){q.query.action="save";};
  };
  if(q.query && q.query.action && (filename.indexOf('mydata')==0) && (filename.indexOf('..')==-1))
  {
   filename=path.join(settings.home,filename.replace('mydata',''));
   switch(q.query.action)
   {
   case "save":
    if(req.method=="POST")
    {
     req.on('data', function (data) {
       body += data;
       if (body.length > 1e6) { 
                // FLOOD ATTACK OR FAULTY CLIENT, NUKE REQUEST
         req.connection.destroy();
         console.log("Possible flood attack");
       }
     });
     req.on('end', function () {
       try
       {
        fs.writeFile(filename,body, function(err) {
         if(err) { sendBack(err,"Write failed to "+q.pathname); }
         else{sendBack("","Write succeeded to "+q.pathname); }
        });
       }
       catch(e)
       {
	    sendBack(e,"Write failed to "+q.pathname);
       };
     });
    };
    return;
   case "createDir":
    fs.mkdir(filename,err => {if(err){sendBack(err,"Failed to create "+q.pathname);}else{sendBack("","Created "+q.pathname)}});
    return;
   case "delete":
    try
    {
      fs.stat(filename,(e,s) => {
        if(e){return sendBack(e,"File not found "+q.pathname);};
        if(s.isDirectory())
        {
    	 fs.rmdir(filename,err => {
    	   if(err){return sendBack(err,"Failed to remove dir "+q.pathname);}
    	   sendBack("","Deleted "+q.pathname);
    	 });
        }
        else
        {
    	 fs.unlink(filename,err => {
    	   if(err){return sendBack(err,"Failed to delete "+q.pathname);}
    	   sendBack("","Deleted "+q.pathname);
    	 });
    	};
      });
    }
    catch(e)
    {
     sendBack(e,"Failed to find "+q.pathname);
    };
    return;
   case "rename":
    let f_to=path.join(".",url.parse(q.query.to,true).pathname);
    if((f_to.indexOf('mydata')==0) && (f_to.indexOf('..')==-1))
    {
     f_to=path.join(settings.home,f_to.replace('mydata',''));
     fs.rename(filename,f_to,err => {if(err){sendBack("","Failed to rename "+q.pathname);}else{sendBack("","Renamed "+q.pathname+" to "+q.query.to)}});
    }
    else
    {
     sendBack("Cannot rename:",filename+" to "+f_to);
    };
    return;
   case "oxcal":
    if(q.query.lock=='true')
    {
     exec('"'+settings.oxcal+'" "'+filename+'"',function (err,stdout,stderr)
      {
       if(false){sendBack(err,stdout+"\n"+stderr)}
       else{sendBack("",stdout+"\n"+stderr)};
      });
    }
    else
    {
     exec('"'+settings.oxcal+'" "'+filename+'"');
     sendBack("","OxCal launched");
    };
    return;
   case "download":
    fs.readFile(filename, function(err, data) {
     if (err) {
       res.writeHead(404,{});
       return res.end("404 Not Found")
     };
     res.writeHead(200, {'Content-Type': mimeType(path.extname(filename)),
		'Content-Disposition': 'attachment; filename='+path.basename(filename),
		'Cache-Control':PRIVATE});
     res.write(data);
     return res.end();
    });
    return;
   case "readDir":
    readDir(filename,q.query.ext,q.query.contains,q.query.within,q.query.detail,function(rtn){
      res.writeHead(200, {'Content-Type': mimeType("json"),
		'Cache-Control':PRIVATE});
      res.write(JSON.stringify(rtn));
      return res.end();
    });
    break;
   };
  }
  else
  {
   let trans=false;
   if((filename.indexOf('mydata')==0) && (filename.indexOf('..')==-1))
   {
    filename=path.join(settings.home,filename.replace('mydata',''));
    trans=true;
   };
   fs.readFile(filename, function(err, data) {
    if (err) {
      res.writeHead(404, {'Content-Type': mimeType(path.extname(filename))});
      return res.end("404 Not Found")
    };
	if(trans)
	{
     res.writeHead(200, {'Content-Type': mimeType(path.extname(filename)),
		'Cache-Control':PRIVATE});
	}
	else
	{
     res.writeHead(200, {'Content-Type': mimeType(path.extname(filename)),
		'Cache-Control':PUBLIC,'Set-Cookie':"NodeJs=true; Max-Age=31536000"});
	};
    res.write(data);
    return res.end();
   });
  };
});