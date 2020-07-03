  function decimalToHex(d, padding) 
  {
   var hex = Number(Math.round(d)).toString(16);
   padding = typeof (padding) === "undefined" || padding === null ? padding = 2 : padding;
   while (hex.length < padding)
   {
    hex = "0" + hex;
   };
   return hex;
  };
  function hsvToRgb(h,s,v)
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
   return "#"+decimalToHex(r,2)+decimalToHex(g,2)+decimalToHex(b,2);
  };
