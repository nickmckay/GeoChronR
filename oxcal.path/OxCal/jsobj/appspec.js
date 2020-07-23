var spec,s,ss,sss,ssss;
spec=new itemSpec("app","Application","Object");

// top level items

s=spec.appendChild("header","Header","Object");
ss=s.appendChild("title","Title","Text");
ss=s.appendChild("id","ID","Text");
ss=s.appendChild("version","Version","Number");
ss=s.appendChild("icon","Icon","Text");
ss=s.appendChild("left_menu","Left menu ID","Text");
ss=s.appendChild("right_menu","Right menu ID","Text");

// application frames
s=spec.appendChild("frames","Frames","Array");
ss=s.appendChild("title","Title","Text");
ss=s.appendChild("icon","Icon","Text");
ss=s.appendChild("left_menu","Left menu ID","Text");
ss.popup=true;
ss=s.appendChild("right_menu","Right menu ID","Text");
ss.popup=true;
ss=s.appendChild("body_type","Body type","Text");
ss.popup=true;
ss.options=["Existing div","New div","Existing iframe","New iframe"];
ss=s.appendChild("body_id","Body ID","Text");
ss.popup=true;
ss=s.appendChild("iframe_src","Iframe src","Text");
ss.popup=true;
ss=s.appendChild("footer_menu","Footer menu ID","Text");
ss.popup=true;
ss=s.appendChild("tools","Tools","Array");
ss.popup=true;
sss=ss.appendChild("tool_id","Tool ID","Text");

// menus

s=spec.appendChild("menus","Menus","Array");
ss=s.appendChild("menu_id","Menu ID","Text");
ss=s.appendChild("items","Menu elements","Array");
sss=ss.appendChild("type","Type","Text");
sss.options=["MenuItem","Spacer","Button","ButtonLeft","ButtonCenter","ButtonRight"];
sss=ss.appendChild("text","Text","Text");
sss=ss.appendChild("title","Hint","Text");
sss=ss.appendChild("icon","Icon","Text");
sss=ss.appendChild("link","Link","Text");
sss=ss.appendChild("onclick","OnClick","Text");
sss=ss.appendChild("submenu_id","Submenu ID","Text");

// tools

s=spec.appendChild("tools","Tools","Array");
ss=s.appendChild("tool_id","Tool ID","Text");
ss=s.appendChild("left","Left","Number");
ss=s.appendChild("top","Top","Number");
ss=s.appendChild("width","Width","Number");
ss=s.appendChild("height","Height","Number");
ss=s.appendChild("body_type","Body type","Text");
ss.popup=true;
ss.options=["Existing div","New div","Existing iframe","New iframe"];
ss=s.appendChild("body_id","Body ID","Text");
ss.popup=true;
ss=s.appendChild("iframe_src","Iframe src","Text");
ss.popup=true;


s=spec.appendChild("options","Options","Object");
ss=s.appendChild("initialise","Initialisation code","Text");
ss=s.appendChild("file_type","File type","Text");
ss=s.appendChild("file_dialog","File dialog","Boolean");
ss=s.appendChild("file_set_name","Set filename","Text");
ss=s.appendChild("file_get_name","Get filename","Text");
ss=s.appendChild("file_set_content","Set file content","Text");
ss=s.appendChild("file_get_content","Get file content","Text");

// required files

s=spec.appendChild("files","JS files","Array");
ss=s.appendChild("filename","Filename","Text");

